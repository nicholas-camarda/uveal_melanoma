import assert from "node:assert/strict";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { pathToFileURL } from "node:url";

const repoRoot = path.resolve(path.dirname(new URL(import.meta.url).pathname), "../..");
const runtimeNodeModules = process.env.RUNTIME_NODE_MODULES;
if (!runtimeNodeModules) throw new Error("RUNTIME_NODE_MODULES must name the bundled dependency directory");
const localModules = path.join(repoRoot, "scripts", "presentations", "node_modules");
let createdModuleLink = false;
try {
  await fs.lstat(localModules);
} catch (error) {
  if (error.code !== "ENOENT") throw error;
  await fs.symlink(runtimeNodeModules, localModules, "dir");
  createdModuleLink = true;
}
const JSZip = (await import(pathToFileURL(path.join(runtimeNodeModules, "jszip", "lib", "index.js")).href)).default;

const builderPath = new URL("../../scripts/presentations/build_objective4_aao_deck.mjs", import.meta.url);
const { buildDeck, validateRuntime } = await import(builderPath.href);
const requiredKeys = ["cohort_total_count", "gep_not_tested_count", "gep_failed_indeterminate_count", "no_gep_scoreable_count", "followup_no_gep_ge_5yr_count", "direct_model_mfs_5yr_lower_count", "direct_model_mfs_5yr_middle_count", "direct_model_mfs_5yr_higher_count", "direct_model_mss_60mo_lower_count", "direct_model_mss_60mo_middle_count", "direct_model_mss_60mo_higher_count"];
const xml = (value) => String(value).replaceAll("&", "&amp;").replaceAll("<", "&lt;");

async function writeWorkbook(file, includePresentationData = true) {
  const zip = new JSZip();
  const headers = ["semantic_id", "section", "group", "label", "value_numeric", "value_character", "unit", "reader_role"];
  const values = { cohort_total_count: 260, gep_not_tested_count: 162, gep_failed_indeterminate_count: 13, no_gep_scoreable_count: 164, followup_no_gep_ge_5yr_count: 83, direct_model_mfs_5yr_lower_count: 55, direct_model_mfs_5yr_middle_count: 55, direct_model_mfs_5yr_higher_count: 54, direct_model_mss_60mo_lower_count: 55, direct_model_mss_60mo_middle_count: 55, direct_model_mss_60mo_higher_count: 54 };
  const rows = requiredKeys.map((id) => [id, "test", "All", id, values[id], "", "count", "reader-safe aggregate"]);
  const sheet = (values) => `<?xml version="1.0"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>${values.map((row, r) => `<row r="${r + 1}">${row.map((cell, c) => `<c r="${String.fromCharCode(65 + c)}${r + 1}" t="inlineStr"><is><t>${xml(cell)}</t></is></c>`).join("")}</row>`).join("")}</sheetData></worksheet>`;
  const sheets = includePresentationData ? ["Presentation_Data", "Model_Performance"] : ["Model_Performance"];
  zip.file("xl/workbook.xml", `<?xml version="1.0"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets>${sheets.map((name, i) => `<sheet name="${name}" sheetId="${i + 1}" r:id="rId${i + 1}"/>`).join("")}</sheets></workbook>`);
  zip.file("xl/_rels/workbook.xml.rels", `<?xml version="1.0"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">${sheets.map((_, i) => `<Relationship Id="rId${i + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet${i + 1}.xml"/>`).join("")}</Relationships>`);
  sheets.forEach((name, i) => zip.file(`xl/worksheets/sheet${i + 1}.xml`, sheet(name === "Presentation_Data" ? [headers, ...rows] : [["metric", "value"], ["surrogate_auc", "0.563"]])));
  await fs.writeFile(file, await zip.generateAsync({ type: "nodebuffer" }));
}

async function fixtureRoot({ gateStatus = "review", comparatorStatus = "pass", includeSheet = true } = {}) {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), "objective4-deck-fixture-"));
  const report = path.join(root, "Analysis", "uveal_full", "04_GEP_Validation", "d_exploratory_no_gep");
  const plots = path.join(report, "plots");
  await fs.mkdir(plots, { recursive: true });
  await fs.writeFile(path.join(root, "objective4-aao-gate.json"), JSON.stringify({ gate_version: 1, status: gateStatus, accepted_abstract_id: "30085896", accepted_submitted_cohort_n: 260, candidate_cohort_n: 260, comparisons: { auc: { molecular_surrogate: { accepted: 0.515, candidate: 0.563, absolute_delta: 0.048 }, direct_mfs: { accepted: 0.686, candidate: 0.656, absolute_delta: 0.03 }, direct_mss: { accepted: 0.663, candidate: 0.603, absolute_delta: 0.06 } } }, reasons: gateStatus === "review" ? [{ id: "auc_delta_review", status: "review", message: "Absolute AUC change requires documented review" }] : [] }));
  await fs.writeFile(path.join(root, "production-comparison.json"), JSON.stringify({ comparator_version: 1, contract_version: 1, status: comparatorStatus, comparisons: [{ id: "objective4-no-gep-validation", status: "pass" }] }));
  await writeWorkbook(path.join(report, "full_cohort_exploratory_no_gep_report.xlsx"), includeSheet);
  await fs.writeFile(path.join(report, "full_cohort_exploratory_no_gep_summary.md"), "Aggregate-only validated Objective 4 summary.");
  const pixel = Buffer.from("iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQIHWP4z8DwHwAFgAI/ScL9/wAAAABJRU5ErkJggg==", "base64");
  for (const figure of ["subgroup_comparison", "observed_risk_thirds", "direct_model_contributors"]) await fs.writeFile(path.join(plots, `full_cohort_exploratory_no_gep_${figure}.png`), pixel);
  return root;
}

await assert.rejects(() => validateRuntime("/path/that/must/not/be-searched"), /gate.*missing|runtime root|required/i);
const failedGateRoot = await fixtureRoot({ gateStatus: "fail" });
await assert.rejects(() => validateRuntime(failedGateRoot), /gate.*reviewable|gate.*status/i);
const malformedGateRoot = await fixtureRoot();
await fs.writeFile(path.join(malformedGateRoot, "objective4-aao-gate.json"), "{not-json");
await assert.rejects(() => validateRuntime(malformedGateRoot), /gate.*malformed/i);
const failedComparatorRoot = await fixtureRoot({ comparatorStatus: "fail" });
await assert.rejects(() => validateRuntime(failedComparatorRoot), /comparator.*pass/i);
const emptyComparatorRoot = await fixtureRoot();
await fs.writeFile(path.join(emptyComparatorRoot, "production-comparison.json"), JSON.stringify({ comparator_version: 1, contract_version: 1, status: "pass", comparisons: [] }));
await assert.rejects(() => validateRuntime(emptyComparatorRoot), /populated comparison/i);
const emptyReviewRoot = await fixtureRoot();
await fs.writeFile(path.join(emptyReviewRoot, "objective4-aao-gate.json"), JSON.stringify({ gate_version: 1, status: "review", accepted_abstract_id: "30085896", accepted_submitted_cohort_n: 260, candidate_cohort_n: 260, comparisons: { auc: { molecular_surrogate: { accepted: 0.515, candidate: 0.563, absolute_delta: 0.048 }, direct_mfs: { accepted: 0.686, candidate: 0.656, absolute_delta: 0.03 }, direct_mss: { accepted: 0.663, candidate: 0.603, absolute_delta: 0.06 } } }, reasons: [] }));
await assert.rejects(() => validateRuntime(emptyReviewRoot), /gate.*reviewable/i);
const missingSheetRoot = await fixtureRoot({ includeSheet: false });
await assert.rejects(() => validateRuntime(missingSheetRoot), /Presentation_Data|sheet/i);
const validRoot = await fixtureRoot();
const reportPath = path.join(validRoot, "Analysis", "uveal_full", "04_GEP_Validation", "d_exploratory_no_gep", "full_cohort_exploratory_no_gep_report.xlsx");
await writeWorkbook(reportPath, true);
const workbook = await JSZip.loadAsync(await fs.readFile(reportPath));
const presentationSheet = workbook.file("xl/worksheets/sheet1.xml");
workbook.file("xl/worksheets/sheet1.xml", (await presentationSheet.async("string")).replace("direct_model_mss_60mo_higher_count", "missing_semantic_key"));
await fs.writeFile(reportPath, await workbook.generateAsync({ type: "nodebuffer" }));
await assert.rejects(() => validateRuntime(validRoot), /semantic key/i);
await writeWorkbook(reportPath, true);
await assert.rejects(() => buildDeck({ runtimeRoot: validRoot, outputPptx: path.join(validRoot, "deck.pptx"), disclosureText: "FINAL REVIEWED DISCLOSURE TEXT" }), /disclosure/i);
const deckPath = path.join(validRoot, "deck.pptx");
await buildDeck({ runtimeRoot: validRoot, outputPptx: deckPath, disclosureText: "No financial relationships to disclose." });
const pptx = await JSZip.loadAsync(await fs.readFile(deckPath));
const presentationXml = await pptx.file("ppt/presentation.xml").async("string");
assert.match(presentationXml, /cx="12192000" cy="6858000"/);
assert.equal((presentationXml.match(/<p:sldId /g) ?? []).length, 10);
const allXml = (await Promise.all(Object.values(pptx.files).filter((file) => /^ppt\/(slides\/slide|notesSlides\/notesSlide)\d+\.xml$/.test(file.name)).map((file) => file.async("string")))).join("\n");
for (const title of ["Baseline Clinical Risk Stratification When Gene Expression Profiling Is Unavailable in Uveal Melanoma", "Financial disclosure", "Two in three patients had no usable molecular risk result", "No GEP is not one patient group", "Clinical data answer a different question", "Which clinical features drove risk ordering", "The models identify a higher-risk group", "What the validation can and cannot support", "Bottom line", "References"]) assert.match(allXml, new RegExp(title, "i"));
for (const metadata of ["Nicholas D. Camarda", "Timothy J. Marquis", "Simon D. Archambault", "Shilpa J. Desai", "Tufts Medical Center"]) assert.match(allXml, new RegExp(metadata, "i"));
assert.match(allXml, /175\/260/);
assert.match(allXml, /67\.3%/);
assert.match(allXml, /0\.563/);
assert.equal((allXml.match(/\[Sources\]/g) ?? []).length >= 8, true);
assert.doesNotMatch(allXml, /significant predictor|patient[_ -]?id|medical record/i);
assert.doesNotMatch(allXml, /typeface|fontFamily/i);
console.log("Objective 4 AAO deck builder contract tests passed.");
if (createdModuleLink) await fs.rm(localModules, { recursive: true, force: true });
