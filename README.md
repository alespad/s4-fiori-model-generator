# Fiori Programming Model Analyzer for S/4HANA – ABAP Generator

This repository contains the **ABAP generator** component of the [Fiori Programming Model Analyzer](https://github.com/alespad/s4-fiori-model-analyzer) project.


**SAP Community Blog Post:** [Is It RAP or BOPF? Fiori Programming Model Analyzer for S/4HANA](https://community.sap.com/t5/abap-blog-posts/is-it-rap-or-bopf-fiori-programming-model-analyzer-for-s-4hana/ba-p/14240651)


## Overview

The **ABAP generator** analyzes **Fiori apps** on your S/4HANA system and automatically classifies them based on key technical indicators:

| Category | Description |
|-----------|--------------|
| **Programming Model** | RAP, BOPF, or N/A (Gateway Classic) |
| **Business Entity** | Root CDS view or Business Object name |
| **OData Version** | V2 or V4 |
| **Service Information** | Service name, URI, SEGW project |
| **FPM Extensions** | Whether the app uses Flexible Programming Model extensions |

The analyzer reads each app’s `manifest.json`, cross-references it with DDL sources and Service Definitions, and generates **JSON** and **CSV** outputs for visualization using the [viewer application](https://github.com/alespad/s4-fiori-model-analyzer).

## Installation

1. **Clone** this repository  
2. **Import the ABAP objects** into your S/4HANA system using [abapGit](https://abapgit.org) 
3. **Activate all objects**

# Usage
## for Standard SAP Fiori Apps

### Report: `Z_FIORI_MODEL_GENERATOR`

The analyzer requires an input **CSV file** exported from the [SAP Fiori Apps Reference Library](https://fioriappslibrary.hana.ondemand.com/).

### Option 1: Export from SAP Fiori Library

1. Go to the **SAP Fiori Apps Reference Library**  
2. Filter by your **S/4HANA version**  
3. Export the **catalog as CSV**

**Required columns:**
- `FioriId`
- `AppName`
- `BSPName`
- `Link`

> Column order doesn’t matter.  
> OData service information is *not* required — it will be extracted automatically from the `manifest.json`.

### Option 2: Use Pre-exported Data

You can also download **ready-to-use CSV files** from the [Viewer Project](https://github.com/alespad/s4-fiori-model-analyzer/tree/main/docs/data). FioriLibraryExport_*.csv

### Report Parameters

| Parameter | Description |
|------------|-------------|
| **Import File** | Path to your CSV file (e.g. `/tmp/fiori_apps.csv`) |
| **Filter by IDs (optional)** | Comma-separated list of Fiori IDs to analyze specific apps |
| **Execute on Local or Server** | Choose execution mode |


### Output

The report generates both **JSON** and **CSV** files, **JSON** can be:

- **Visualized** locally using the [Viewer Application](https://github.com/alespad/s4-fiori-model-analyzer)  
 → See [*“Using Your Own Data”*](https://github.com/alespad/s4-fiori-model-analyzer?tab=readme-ov-file#using-your-own-data) section in the viewer repository.
- **Contributed** back to the main project via Pull Request  
  → Update `sources.json` and add your CSV/JSON files  
  → Help expand coverage for additional **S/4HANA releases**

## for Analyzing Custom Fiori Apps

Analyze **custom BSP Fiori applications** deployed on your S/4HANA system using a **RAP Custom Entity**.

### CDS Custom Entity: `ZI_FIORI_CUST_MODEL`

This CDS view reads BSP applications directly from the system's **TADIR** table and analyzes them using the same classification engine used for standard Fiori apps.

### How to Use

**Option 1: ADT Preview**
1. Open Eclipse ADT
2. Navigate to `ZI_FIORI_CUST_MODEL`
3. Right-click → **Open With** → **Data Preview**
4. Use the filter fields to narrow down results

**Option 2: OData Service**
1. Activate Service Binding `ZSRV_FIORI_CUST_MODEL_ANALYZ`
2. Access via OData V4 endpoint

### Filter Criteria

| Field | Description | Example |
|-------|-------------|---------|
| **BspName** | BSP application name (supports wildcards `*`) | `Z*`, `ZCUST_APP` |
| **Author** | Developer user ID | `DEVELOPER1` |
| **Devclass** | Package name | `ZCUSTOM_PKG` |

### Output Fields

| Field | Description |
|-------|-------------|
| **BspName** | BSP application name (key) |
| **Devclass** | Package |
| **Author** | Creator |
| **ProgrammingModel** | RAP, BOPF, or N/A |
| **OdataVersion** | 2.0 or 4.0 |
| **BusinessEntity** | Root CDS view or BO name |
| **MainServiceName** | OData service name |
| **ServiceUri** | Service endpoint URI |
| **SegwProject** | SEGW project (if V2) |
| **FpmExtended** | Yes/No/N/A |
| **AppName** | Application name from manifest |

### Technical Components

| Object | Type | Description |
|--------|------|-------------|
| `ZI_FIORI_CUST_MODEL` | CDS Custom Entity | Main view with UI annotations |
| `ZCL_FIORI_CUST_MODEL_QUERY` | Class | RAP Query Provider (IF_RAP_QUERY_PROVIDER) |
| `ZSD_FIORI_CUST_MODEL_ANALYZ` | Service Definition | Exposes the custom entity |
| `ZSRV_FIORI_CUST_MODEL_ANALYZ` | Service Binding | OData V4 binding |

> **Note:** BSP applications without a valid `manifest.json` are automatically filtered out (they are not Fiori apps).

## How it Works (Technical Overview)
The analyzer uses two main ABAP classes to determine the programming model behind Fiori apps:
### Class ZCL_FIORI_MODEL_MANIFEST
This class is responsible for extracting information from the app's manifest.json file. Important note: The manifest is NOT parsed into JSON structures, but analyzed using string operations and pattern matching. This design choice was necessary because manifest structures vary significantly across different Fiori app types (Fiori Elements V2/V4, freestyle apps, apps with extensions, etc.)

The class extracts:
- The main OData service name and URI
- The primary entity set used by the app
- OData version (V2 or V4)
- Data source configuration

### Class ZCL_FIORI_MODEL_ANALYZER
This is the core engine that performs the classification. For each app, it:

- Retrieves the manifest using the BSP application name
- Determines the OData version by checking SEGW projects (V2) or Service Bindings (V4)
- Resolves CDS views by reading the DDL source code:
    - Identifies the Consumption view (C_*)
    - Extracts the Interface view (I_*) from the FROM clause
- Classifies the programming model using multiple detection strategies:
    - RAP: Looks for root view entity, projections on R_* views, VDM consumption patterns
    - BOPF: Detects BOPF annotations (@ObjectModel.modelCategory: #BOPF, transactionalProcessingEnabled) by navigating through the CDS view hierarchy
    - N/A: Gateway Classic (non-managed scenarios)
- Detects FPM extensions by searching for controller/view extensions in the manifest

## What's next
- Improve *N/A* classification logic
- Some SEGW Projects are not determined
- Validate accuracy across different S/4HANA releases  

---
 **Credits**  
- Code versioning & distribution via [**abapGit**](https://abapgit.org) ([contributors](https://abapgit.org/sponsor.html))  
- Static code checks via [**abaplint**](https://abaplint.app) ([contributors](https://github.com/abaplint/abaplint/graphs/contributors))  
