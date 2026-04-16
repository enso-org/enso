import * as agC from 'ag-grid-community'
import { ModuleRegistry } from 'ag-grid-community'
import * as agE from 'ag-grid-enterprise'
import { LicenseManager } from 'ag-grid-enterprise'
export { AgGridVue } from 'ag-grid-vue3'

if (typeof $config.AG_GRID_LICENSE_KEY !== 'string') {
  console.warn('The AG_GRID_LICENSE_KEY is not defined.')
  if (import.meta.env.DEV) {
    // Hide annoying license validation errors in dev mode when the license is not defined. The
    // missing define warning is still displayed to not forget about it, but it isn't as obnoxious.
    const origValidateLicense = LicenseManager.prototype.validateLicense
    LicenseManager.prototype.validateLicense = function (this) {
      if (!('licenseManager' in this))
        Object.defineProperty(this, 'licenseManager', {
          configurable: true,
          set(value: any) {
            Object.getPrototypeOf(value).validateLicense = () => {}
            delete this.licenseManager
            this.licenseManager = value
          },
        })
      origValidateLicense.call(this)
    }
  }
} else {
  LicenseManager.setLicenseKey($config.AG_GRID_LICENSE_KEY)
}

ModuleRegistry.registerModules([
  agC.BigIntFilterModule,
  agC.CellStyleModule,
  agC.ClientSideRowModelApiModule,
  agC.ClientSideRowModelModule,
  agC.ColumnApiModule,
  agC.ColumnAutoSizeModule,
  agC.ColumnHoverModule,
  agC.DateFilterModule,
  agC.EventApiModule,
  agC.NumberFilterModule,
  agC.RenderApiModule,
  agC.RowAutoHeightModule,
  agC.RowDragModule,
  agC.TextEditorModule,
  agC.TextFilterModule,
  agC.TooltipModule,
  agE.CellSelectionModule,
  agE.ClipboardModule,
  agE.ColumnMenuModule,
  agE.ContextMenuModule,
  agE.CsvExportModule,
  agE.ExcelExportModule,
  agE.MultiFilterModule,
  agE.ServerSideRowModelApiModule,
  agE.ServerSideRowModelModule,
  agE.SetFilterModule,
  agE.StatusBarModule,
])
if (import.meta.env.DEV) {
  ModuleRegistry.registerModules([agC.ValidationModule])
}
