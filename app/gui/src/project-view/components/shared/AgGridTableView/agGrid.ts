import * as ag from 'ag-grid-enterprise'
import { LicenseManager, ModuleRegistry } from 'ag-grid-enterprise'
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
  ag.BigIntFilterModule,
  ag.CellSelectionModule,
  ag.CellStyleModule,
  ag.ClientSideRowModelApiModule,
  ag.ClientSideRowModelModule,
  ag.ClipboardModule,
  ag.ColumnApiModule,
  ag.ColumnAutoSizeModule,
  ag.ColumnHoverModule,
  ag.ColumnMenuModule,
  ag.ContextMenuModule,
  ag.CsvExportModule,
  ag.DateFilterModule,
  ag.EventApiModule,
  ag.ExcelExportModule,
  ag.GridStateModule,
  ag.MultiFilterModule,
  ag.NumberFilterModule,
  ag.RenderApiModule,
  ag.RowAutoHeightModule,
  ag.RowDragModule,
  ag.ServerSideRowModelApiModule,
  ag.ServerSideRowModelModule,
  ag.SetFilterModule,
  ag.StatusBarModule,
  ag.TextEditorModule,
  ag.TextFilterModule,
  ag.TooltipModule,
])
if (import.meta.env.DEV) {
  ModuleRegistry.registerModules([ag.ValidationModule])
}
