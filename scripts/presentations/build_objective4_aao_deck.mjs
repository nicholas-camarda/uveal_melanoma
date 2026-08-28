import fs from "node:fs/promises";
import path from "node:path";
import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";

const runtimeNodeModules = process.env.RUNTIME_NODE_MODULES;
if (!runtimeNodeModules) throw new Error("RUNTIME_NODE_MODULES must name the bundled dependency directory");
const runtimeRequire = createRequire(path.join(runtimeNodeModules, "objective4-aao-deck-loader.cjs"));
const JSZip = runtimeRequire("jszip");
const { Presentation, PresentationFile } = await import(pathToFileURL(runtimeRequire.resolve("@oai/artifact-tool")).href);

const REPORT_RELATIVE = path.join("Analysis", "uveal_full", "04_GEP_Validation", "d_exploratory_no_gep");
const FIGURE_NAMES = ["subgroup_comparison", "observed_risk_thirds", "direct_model_contributors"];
const REQUIRED_SEMANTIC_IDS = ["cohort_total_count", "gep_not_tested_count", "gep_failed_indeterminate_count", "no_gep_scoreable_count", "followup_no_gep_ge_5yr_count", "direct_model_mfs_5yr_lower_count", "direct_model_mfs_5yr_middle_count", "direct_model_mfs_5yr_higher_count", "direct_model_mss_60mo_lower_count", "direct_model_mss_60mo_middle_count", "direct_model_mss_60mo_higher_count"];
const REQUIRED_COMPARISON_IDS = ["objective4-no-gep-validation", "objective4-mfs-sensitivity", "objective4-mfs-consolidated", "objective4-mss-consolidated", "objective4-gep-distribution"];
const BLACK = "#000000";
const BLUE = "#1F4E79";
const GRAY = "#5B6573";

function assertObject(value, label) {
  if (!value || typeof value !== "object" || Array.isArray(value)) throw new Error(`${label} is malformed`);
  return value;
}

async function readJson(file, label) {
  try { return assertObject(JSON.parse(await fs.readFile(file, "utf8")), label); }
  catch (error) { throw new Error(`${label} is missing or malformed: ${error.message}`); }
}

function columnIndex(reference) {
  return [...reference.replace(/\d/g, "")].reduce((total, letter) => total * 26 + letter.charCodeAt(0) - 64, 0) - 1;
}

async function readWorkbook(file) {
  const zip = await JSZip.loadAsync(await fs.readFile(file));
  const workbook = await zip.file("xl/workbook.xml")?.async("string");
  const rels = await zip.file("xl/_rels/workbook.xml.rels")?.async("string");
  if (!workbook || !rels) throw new Error("workbook structure is malformed");
  const shared = await zip.file("xl/sharedStrings.xml")?.async("string");
  const strings = shared ? [...shared.matchAll(/<si>([\s\S]*?)<\/si>/g)].map((match) => [...match[1].matchAll(/<t[^>]*>([\s\S]*?)<\/t>/g)].map((part) => part[1]).join("")) : [];
  const relationships = new Map([...rels.matchAll(/<Relationship\b[^>]*Id="([^"]+)"[^>]*Target="([^"]+)"/g)].map((match) => [match[1], `xl/${match[2]}`]));
  const tables = {};
  for (const match of workbook.matchAll(/<sheet\b[^>]*name="([^"]+)"[^>]*r:id="([^"]+)"[^>]*\/>/g)) {
    const xml = await zip.file(relationships.get(match[2]))?.async("string");
    if (!xml) continue;
    const rows = [];
    for (const row of xml.matchAll(/<row\b[^>]*>([\s\S]*?)<\/row>/g)) {
      const values = [];
      for (const cell of row[1].matchAll(/<c\b([^>]*)>([\s\S]*?)<\/c>/g)) {
        const ref = /r="([^"]+)"/.exec(cell[1])?.[1];
        if (!ref) continue;
        const raw = /<v>([\s\S]*?)<\/v>/.exec(cell[2])?.[1] ?? /<t[^>]*>([\s\S]*?)<\/t>/.exec(cell[2])?.[1] ?? "";
        values[columnIndex(ref)] = /t="s"/.test(cell[1]) ? strings[Number(raw)] : raw;
      }
      rows.push(values.map((value) => value ?? ""));
    }
    tables[match[1]] = rows;
  }
  return tables;
}

function disclosureIsResolved(text) {
  return typeof text === "string" && text.trim().length >= 8 && !/placeholder|final reviewed|tbd|to be (confirmed|determined)|author review/i.test(text);
}

export async function validateRuntime(runtimeRoot) {
  if (!runtimeRoot || !path.isAbsolute(runtimeRoot)) throw new Error("A single absolute --runtime-root is required; no fallback runtime is permitted");
  const root = path.resolve(runtimeRoot);
  const gate = await readJson(path.join(root, "objective4-aao-gate.json"), "Objective 4 AAO gate");
  const comparator = await readJson(path.join(root, "production-comparison.json"), "production comparator");
  const aucEntries = ["molecular_surrogate", "direct_mfs", "direct_mss"].map((key) => gate.comparisons?.auc?.[key]);
  const gateAucsAreComplete = aucEntries.every((entry) =>
    entry && [entry.accepted, entry.candidate, entry.absolute_delta].every(Number.isFinite)
  );
  const reviewReasonsAreComplete = gate.status !== "review" || (
    Array.isArray(gate.reasons) && gate.reasons.length > 0 && gate.reasons.every((reason) =>
      reason && reason.status === "review" && typeof reason.id === "string" && reason.id.length > 0 &&
      typeof reason.message === "string" && reason.message.length > 0
    )
  );
  if (
    gate.gate_version !== 1 ||
      !["pass", "review"].includes(gate.status) ||
      gate.accepted_abstract_id !== "30085896" ||
      !Number.isInteger(gate.accepted_submitted_cohort_n) ||
      !Number.isInteger(gate.candidate_cohort_n) ||
      gate.accepted_submitted_cohort_n !== gate.candidate_cohort_n ||
      !gateAucsAreComplete ||
      !reviewReasonsAreComplete
  ) throw new Error("AAO gate is not structurally reviewable");
  if (
    comparator.comparator_version !== 1 ||
      comparator.contract_version !== 1 ||
      comparator.status !== "pass" ||
      !Array.isArray(comparator.comparisons) ||
      comparator.comparisons.length === 0 ||
      comparator.comparisons.some((comparison) =>
        !comparison || comparison.status !== "pass" || typeof comparison.id !== "string" || comparison.id.length === 0
      )
  ) throw new Error("production comparator must pass populated comparison entries");
  const comparisonIds = new Set(comparator.comparisons.map((comparison) => comparison.id));
  for (const requiredId of REQUIRED_COMPARISON_IDS) {
    if (!comparisonIds.has(requiredId)) throw new Error(`required production comparison is missing: ${requiredId}`);
  }
  const reportDir = path.join(root, REPORT_RELATIVE);
  const workbookPath = path.join(reportDir, "full_cohort_exploratory_no_gep_report.xlsx");
  const summaryPath = path.join(reportDir, "full_cohort_exploratory_no_gep_summary.md");
  const [tables, summary] = await Promise.all([readWorkbook(workbookPath), fs.readFile(summaryPath, "utf8")]);
  const presentationData = tables.Presentation_Data;
  if (!presentationData?.length) throw new Error("Presentation_Data sheet is required");
  const headers = presentationData[0];
  const semanticIndex = headers.indexOf("semantic_id");
  const valueIndex = headers.indexOf("value_numeric");
  if (semanticIndex < 0 || valueIndex < 0) throw new Error("Presentation_Data semantic columns are required");
  const values = new Map(presentationData.slice(1).map((row) => [row[semanticIndex], Number(row[valueIndex])]));
  for (const key of REQUIRED_SEMANTIC_IDS) if (!Number.isFinite(values.get(key))) throw new Error(`Presentation_Data semantic key is missing or invalid: ${key}`);
  const figures = Object.fromEntries(await Promise.all(FIGURE_NAMES.map(async (name) => {
    const file = path.join(reportDir, "plots", `full_cohort_exploratory_no_gep_${name}.png`);
    await fs.access(file); return [name, file];
  })));
  return { root, gate, comparator, workbookPath, summaryPath, summary, figures, values };
}

function addText(slide, text, position, style = {}) {
  const shape = slide.shapes.add({ geometry: "textbox", position, fill: "none", line: { style: "solid", fill: "none", width: 0 } });
  shape.text = text;
  shape.text.style = { fontSize: 22, color: BLACK, ...style };
  return shape;
}
function addTitle(slide, title) { addText(slide, title, { left: 70, top: 50, width: 1140, height: 62 }, { fontSize: 38, bold: true }); }
function addNote(slide, source) { slide.speakerNotes.textFrame.setText(`[Sources]\n${source}`); slide.speakerNotes.setVisible(true); }
async function addFigure(slide, file, position) { const bytes = await fs.readFile(file); slide.images.add({ blob: bytes.buffer.slice(bytes.byteOffset, bytes.byteOffset + bytes.byteLength), contentType: "image/png", alt: "Generated aggregate Objective 4 figure", fit: "contain", position }); }
function pct(value) { return `${(value * 100).toFixed(1)}%`; }

export async function buildDeck({ runtimeRoot, outputPptx, disclosureText }) {
  if (!disclosureIsResolved(disclosureText)) throw new Error("Disclosure text is unresolved; provide final reviewed disclosure text");
  if (!outputPptx || !path.isAbsolute(outputPptx)) throw new Error("An absolute --output-pptx is required");
  const data = await validateRuntime(runtimeRoot);
  const deck = Presentation.create({ slideSize: { width: 1280, height: 720 } });
  const slide = () => { const next = deck.slides.add(); next.background.fill = "#FFFFFF"; return next; };
  let s = slide(); addText(s, "Baseline Clinical Risk Stratification When Gene Expression Profiling Is Unavailable in Uveal Melanoma", { left: 70, top: 105, width: 1120, height: 180 }, { fontSize: 52, bold: true }); addText(s, "Nicholas D. Camarda, MD, PhD; Timothy J. Marquis, MD; Simon D. Archambault, MD, MSc; Shilpa J. Desai, MD", { left: 74, top: 315, width: 1120, height: 55 }, { fontSize: 18 }); addText(s, "Department of Ophthalmology, Tufts Medical Center, Boston, Massachusetts\nAAO 2026 Poster Discussion • PO203 • Abstract 30085896", { left: 74, top: 385, width: 900, height: 80 }, { fontSize: 22, color: GRAY }); addNote(s, "Accepted AAO abstract PO203; generated Objective 4 report Start_Here.");
  s = slide(); addTitle(s, "Financial disclosure"); addText(s, disclosureText, { left: 90, top: 210, width: 1050, height: 130 }, { fontSize: 30 }); addNote(s, "Final author-reviewed AAO disclosure text.");
  s = slide(); addTitle(s, "Two in three patients had no usable molecular risk result"); const total = data.values.get("cohort_total_count"); const notTested = data.values.get("gep_not_tested_count"); const failed = data.values.get("gep_failed_indeterminate_count"); const unavailable = notTested + failed; addText(s, `${unavailable}/${total}\n${pct(unavailable / total)}\nno usable GEP`, { left: 100, top: 150, width: 450, height: 250 }, { fontSize: 52, bold: true, color: BLUE }); addText(s, `${notTested} not tested\n${failed} failed or indeterminate\n\nMolecular information gaps affect counseling and surveillance.`, { left: 650, top: 205, width: 430, height: 220 }, { fontSize: 26 }); addNote(s, "Workbook: Presentation_Data (cohort_total_count, gep_not_tested_count, gep_failed_indeterminate_count); generated summary.");
  s = slide(); addTitle(s, "No GEP is not one patient group"); await addFigure(s, data.figures.subgroup_comparison, { left: 105, top: 135, width: 1070, height: 470 }); addText(s, "Descriptive differences do not explain why testing was omitted or failed. Failed/indeterminate results are based on a small group.", { left: 105, top: 620, width: 1070, height: 50 }, { fontSize: 18 }); addNote(s, "Figure: full_cohort_exploratory_no_gep_subgroup_comparison.png; workbook: No_GEP_Subgroups and Presentation_Data.");
  s = slide(); addTitle(s, "Clinical data answer a different question"); const surrogateAuc = data.gate.comparisons?.auc?.molecular_surrogate?.candidate; if (!Number.isFinite(surrogateAuc)) throw new Error("AAO gate is missing the molecular-surrogate candidate AUC"); addText(s, "Can clinical features approximate molecular Class 2 status?", { left: 105, top: 180, width: 440, height: 90 }, { fontSize: 26, bold: true }); addText(s, `Out-of-fold AUC ${surrogateAuc.toFixed(3)}\nNot reliably enough to relabel molecular class.`, { left: 105, top: 295, width: 420, height: 110 }, { fontSize: 22, color: GRAY }); addText(s, "Can clinical features order group-level outcome risk?", { left: 680, top: 180, width: 450, height: 90 }, { fontSize: 26, bold: true }); addText(s, "Yes—exploratory models estimate metastasis and melanoma-death risk directly.", { left: 680, top: 295, width: 450, height: 100 }, { fontSize: 22, color: GRAY }); addNote(s, "Gate: objective4-aao-gate.json comparisons.auc.molecular_surrogate.candidate; workbook: Model_Performance; generated summary; code: scripts/gep/orchestration/gep_exploratory_no_gep_report.R. Outer folds test patients not used for model selection.");
  s = slide(); addTitle(s, "Which clinical features drove risk ordering"); await addFigure(s, data.figures.direct_model_contributors, { left: 100, top: 135, width: 1080, height: 480 }); addText(s, "Model weights, not causal effects or conventional significance tests.", { left: 100, top: 620, width: 1000, height: 40 }, { fontSize: 20, bold: true }); addNote(s, "Figure: full_cohort_exploratory_no_gep_direct_model_contributors.png; workbook: Predictor_Contribution.");
  s = slide(); addTitle(s, "The models identify a higher-risk group"); await addFigure(s, data.figures.observed_risk_thirds, { left: 100, top: 130, width: 1080, height: 490 }); addText(s, "Lower-, middle-, and higher-predicted-risk thirds are descriptive groups—not validated clinical categories.", { left: 100, top: 625, width: 1060, height: 38 }, { fontSize: 18 }); addNote(s, "Figure: full_cohort_exploratory_no_gep_observed_risk_thirds.png; workbook: Sensitivity_Pooled_No_GEP and Presentation_Data.");
  s = slide(); addTitle(s, "What the validation can and cannot support"); const scoreable = data.values.get("no_gep_scoreable_count"); const followup = data.values.get("followup_no_gep_ge_5yr_count"); addText(s, `${total} total\n↓\n${notTested + failed} without usable GEP\n↓\n${scoreable} scoreable\n↓\n${followup} with at least 5 years follow-up`, { left: 170, top: 150, width: 420, height: 440 }, { fontSize: 27, bold: true, alignment: "center" }); addText(s, "Out-of-fold evaluation tests held-out patients. Shorter follow-up and the small failed/indeterminate subgroup limit precision.", { left: 700, top: 250, width: 380, height: 170 }, { fontSize: 25 }); addNote(s, "Workbook: Presentation_Data (cohort_total_count, no_gep_scoreable_count, followup_no_gep_ge_5yr_count); generated summary.");
  s = slide(); addTitle(s, "Bottom line"); addText(s, "No usable GEP was common and heterogeneous.\n\nBaseline clinical features supported group-level risk ordering.\n\nClinical models did not recover molecular class and should not replace GEP.\n\nUse estimates as supplementary context pending external validation.", { left: 135, top: 155, width: 980, height: 410 }, { fontSize: 29 }); addNote(s, "Workbook: Presentation_Data, Model_Performance, Sensitivity_Pooled_No_GEP; generated summary.");
  s = slide(); addTitle(s, "References"); addText(s, "1. AAO 2026 accepted abstract PO203.\n2. Validated Objective 4 exploratory no-GEP generated report and summary.\n3. Censoring-aware Kaplan–Meier and competing-risk cumulative-incidence methods.", { left: 100, top: 175, width: 1050, height: 250 }, { fontSize: 24 }); addNote(s, "Accepted abstract PO203; generated report workbook and summary; methods documentation.");
  await fs.mkdir(path.dirname(outputPptx), { recursive: true });
  const pptx = await PresentationFile.exportPptx(deck); await pptx.save(outputPptx);
  return { outputPptx, runtimeRoot: data.root };
}

function parseArgs(args) { const out = {}; for (let i = 0; i < args.length; i += 2) { if (!args[i]?.startsWith("--") || !args[i + 1]) throw new Error("Usage: --runtime-root PATH --output-pptx PATH --disclosure-text TEXT"); out[args[i].slice(2).replace(/-([a-z])/g, (_, c) => c.toUpperCase())] = args[i + 1]; } return out; }
if (import.meta.url === `file://${process.argv[1]}`) buildDeck(parseArgs(process.argv.slice(2))).catch((error) => { console.error(error.message); process.exitCode = 1; });
