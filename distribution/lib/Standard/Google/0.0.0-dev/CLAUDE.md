# Standard.Google

Google Cloud integrations: Google Sheets workbook access, Google Analytics
Reporting API, and Google credentials.

## Main entry points

- `Google_Credential` — credentials backed by an `Enso_Secret`.
- `Google_Sheets` — open a workbook by ID; `Google_Sheets.read id credentials`.
- `Google_Sheets_Workbook` — workbook handle with sheet metadata, named ranges,
  and `read`/`read_many` for individual sheets.
- `Google_Analytics`, `Google_Analytics_Account`, `Google_Analytics_Property`,
  `Google_Analytics_Field` — Analytics Reporting API entry points.

## Common usage

```
from Standard.Google import Google_Sheets

wb = Google_Sheets.read '1ifGXhOlOp2xn4l9bGYbqGvAoSsG5M_M2ga60WFaBZwg' (Enso_Secret.get 'GoogleSheets')
sheet_names = wb.sheet_names
data = wb.read 'Sheet1' ..Has_Headers

multi = wb.read_many ['Sheet1','Sheet2'] ..Has_Headers
```

## Layout

- `src/Google_Sheets.enso` — Sheets entry point.
- `src/Google_Sheets_Workbook.enso` — workbook operations.
- `src/Google_Analytics.enso` — Analytics queries.
- `src/Google_Analytics_Account.enso`, `Google_Analytics_Property.enso`,
  `Google_Analytics_Field.enso` — Analytics metadata types.
- `src/Google_Credential.enso` — credential configuration.

## Things to avoid in generated code

- Ignoring `Duplicate_Output_Column_Names` and `Empty_Sheet` warnings — they
  often indicate the sheet's header row isn't where you expect.

## Where to read more

- `src/Google_Sheets.enso` — Sheets API entry.
- `src/Google_Sheets_Workbook.enso` — workbook details.
- `test/Google_Test/src/` — comprehensive Sheets and Analytics examples.
