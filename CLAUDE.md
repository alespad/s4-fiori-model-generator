# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**S/4 Fiori Model Generator** is an ABAP tool that analyzes SAP Fiori applications deployed on S/4HANA systems and classifies them based on their underlying programming model:
- **RAP** (REST Assured Programming)
- **BOPF** (Business Object Processing Framework)
- **N/A** (Gateway Classic / non-managed scenarios)

The project supports two modes of operation:

1. **Standard Fiori Apps Analysis** (via CSV from Fiori Apps Reference Library)
   - Input: CSV file exported from SAP Fiori Apps Reference Library
   - Output: JSON and CSV files for visualization in companion viewer

2. **Custom Fiori Apps Analysis** (via RAP Custom Entity)
   - Analyzes BSP applications deployed directly on the S/4HANA system
   - Source: TADIR table (PGMID='R3TR', OBJECT='WAPA')
   - Accessible via ADT preview or OData service

## Build & Installation

This is an ABAP project managed with **abapGit**:

1. Clone repository and import into S/4HANA using abapGit (https://abapgit.org)
2. Activate all objects in SAP system
3. For standard analysis: Run report `Z_FIORI_MODEL_GENERATOR` via SE38/SA38
4. For custom apps: Preview CDS `ZI_FIORI_CUST_MODEL` in ADT or activate Service Binding `ZSRVB_FIORI_CUST_MODEL`

## Code Quality

Static code analysis via abaplint (configured in `abaplint.jsonc`):
- Target ABAP syntax version: 7.58
- Custom namespace pattern: `^(Z|Y|LCL_|TY_|LIF_)`

## Architecture

### Standard Analysis Components

**Z_FIORI_MODEL_GENERATOR.prog.abap** - Main entry point for batch analysis
- SAP Report with selection screen for file paths and options
- Dual execution mode: Local PC or Application Server file handling
- Generates CSV and JSON output files

**ZCL_FIORI_MODEL_MANIFEST.clas.abap** - Manifest parser
- Extracts OData service endpoint, version, and primary entity from app manifest.json
- Uses string-based parsing (not JSON deserialization) to handle varied manifest structures
- Key methods: `GET_MAIN_DATASOURCE()`, `GET_PRIMARY_ENTITYSET()`

**ZCL_FIORI_MODEL_ANALYZER.clas.abap** - Core analysis engine (shared by both modes)
- Orchestrates the classification pipeline for each app
- Key public methods:
  - `run_collect()` / `run_collect_from_string()` - Batch process apps
  - `write_output_csv()` / `write_output_json()` - Generate output
  - `analyze_app()` - Main analysis for single app (reused by custom entity)
  - `classify()` - Programming model classification logic
- Detection strategies: checks root entities, BOPF annotations (`@ObjectModel.modelCategory`), VDM consumption patterns, RAP indicators (root views, R_* patterns)

### Custom Apps Analysis Components (RAP)

**ZI_FIORI_CUST_MODEL.ddls** - CDS Custom Entity
- Reads BSP applications from TADIR (custom Fiori apps deployed on system)
- Filter criteria: BspName, Author, Devclass (package)
- Output: Programming model, OData version, Business entity, Service info
- UI annotations for ADT preview

**ZCL_FIORI_CUST_MODEL_QUERY.clas** - RAP Query Provider
- Implements `IF_RAP_QUERY_PROVIDER`
- Queries TADIR for BSP applications (WAPA objects)
- Reuses `ZCL_FIORI_MODEL_ANALYZER=>analyze_app()` for classification
- Filters out non-Fiori BSPs (those without manifest.json)

**ZSD_FIORI_CUST_MODEL.srvd** - Service Definition

**ZSRVB_FIORI_CUST_MODEL.srvb** - Service Binding (OData V2)

### Data Flow

**Standard Analysis:**
```
CSV Input → Parse apps → For each app:
  → Read manifest.json (via BSP)
  → Extract OData service info
  → Query SEGW/Service Binding tables
  → Resolve entity → CDS views (C_* and I_*)
  → Read DDL source, classify model
→ Output JSON + CSV
```

**Custom Apps Analysis:**
```
TADIR Query (WAPA) → Filter by BspName/Author/Devclass → For each BSP:
  → Read manifest.json (via BSP)
  → If no manifest → skip (not a Fiori app)
  → Extract OData service info
  → Classify using same engine
→ Return via RAP Query
```

### Key SAP Dependencies

- `/iwbep/i_sbd_sv` - OData service registry (SEGW)
- `srvb_service_details` - Service Binding details
- `/ui2/cl_json` - JSON serialization
- `TADIR` - Repository objects catalog (for custom apps)
- `IF_RAP_QUERY_PROVIDER` - RAP unmanaged query interface

## Related Projects

- **Companion Viewer**: https://github.com/alespad/s4-fiori-model-analyzer (consumes JSON output)
- **SAP Community Blog**: https://community.sap.com/t5/abap-blog-posts/is-it-rap-or-bopf-fiori-programming-model-analyzer-for-s-4hana/ba-p/14240651
