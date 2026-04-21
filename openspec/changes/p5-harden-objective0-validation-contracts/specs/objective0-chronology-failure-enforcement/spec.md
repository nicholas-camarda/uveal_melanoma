## ADDED Requirements

### Requirement: Objective 0 SHALL hard-fail impossible chronology that changes analytic endpoint timing
Objective 0 MUST NOT silently clamp or normalize impossible chronology when that chronology changes a derived analytic time-to-event or event-status field used by downstream objectives.

#### Scenario: Impossible chronology affects derived survival time
- **WHEN** source dates imply a negative or impossible interval for a derived analytic endpoint
- **THEN** Objective 0 stops the affected cohort from being treated as validation-successful and publishes an explicit hard failure or stop artifact

#### Scenario: Warning-only behavior is reserved for non-analytic chronology gaps
- **WHEN** a chronology issue does not alter any downstream analytic endpoint and remains purely documentary
- **THEN** Objective 0 may record it as a warning with diagnostics instead of escalating it to a hard failure

### Requirement: Chronology failures SHALL be reviewable from published artifacts
Objective 0 chronology failures MUST be accompanied by review artifacts that identify the affected rows, fields, and derived endpoints.

#### Scenario: Chronology failure artifact includes impacted fields
- **WHEN** Objective 0 blocks a cohort because of impossible chronology
- **THEN** the published diagnostics identify the row or record key, the source fields, and the derived endpoint fields affected
