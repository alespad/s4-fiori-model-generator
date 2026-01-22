@EndUserText.label: 'Custom Fiori App Model Analysis'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_FIORI_CUST_MODEL_QUERY'

@UI.headerInfo: {
  typeName: 'Custom Fiori App',
  typeNamePlural: 'Custom Fiori Apps',
  title: { value: 'BspName' },
  description: { value: 'AppName' }
}

define custom entity ZI_FIORI_CUST_MODEL
{
      @UI.facet: [
        { id: 'General', purpose: #STANDARD, type: #IDENTIFICATION_REFERENCE, label: 'General', position: 10 },
        { id: 'Technical', purpose: #STANDARD, type: #FIELDGROUP_REFERENCE, targetQualifier: 'Technical', label: 'Technical Details', position: 20 }
      ]

      @UI: {
        lineItem: [{ position: 10, importance: #HIGH }],
        identification: [{ position: 10 }],
        selectionField: [{ position: 10 }]
      }
      @EndUserText.label: 'BSP Name'
  key BspName           : abap.char(30);

      @UI: {
        lineItem: [{ position: 20, importance: #HIGH }],
        identification: [{ position: 20 }],
        selectionField: [{ position: 20 }]
      }
      @EndUserText.label: 'Package'
      Devclass          : abap.char(30);

      @UI: {
        lineItem: [{ position: 30, importance: #MEDIUM }],
        identification: [{ position: 30 }],
        selectionField: [{ position: 30 }]
      }
      @EndUserText.label: 'Author'
      Author            : abap.char(12);

      @UI: {
        lineItem: [{ position: 40, importance: #HIGH }],
        identification: [{ position: 40 }]
      }
      @EndUserText.label: 'Programming Model'
      ProgrammingModel  : abap.char(10);

      @UI: {
        lineItem: [{ position: 50, importance: #HIGH }],
        identification: [{ position: 50 }],
        fieldGroup: [{ qualifier: 'Technical', position: 10 }]
      }
      @EndUserText.label: 'OData Version'
      OdataVersion      : abap.char(5);

      @UI: {
        lineItem: [{ position: 60, importance: #MEDIUM }],
        identification: [{ position: 60 }],
        fieldGroup: [{ qualifier: 'Technical', position: 20 }]
      }
      @EndUserText.label: 'Business Entity'
      BusinessEntity    : abap.char(80);

      @UI: {
        lineItem: [{ position: 70, importance: #MEDIUM }],
        identification: [{ position: 70 }],
        fieldGroup: [{ qualifier: 'Technical', position: 30 }]
      }
      @EndUserText.label: 'Service Name'
      MainServiceName   : abap.char(80);

      @UI: {
        identification: [{ position: 80 }],
        fieldGroup: [{ qualifier: 'Technical', position: 40 }]
      }
      @EndUserText.label: 'Service URI'
      ServiceUri        : abap.char(255);

      @UI: {
        lineItem: [{ position: 80, importance: #LOW }],
        identification: [{ position: 90 }],
        fieldGroup: [{ qualifier: 'Technical', position: 50 }]
      }
      @EndUserText.label: 'SEGW Project'
      SegwProject       : abap.char(40);

      @UI: {
        lineItem: [{ position: 90, importance: #LOW }],
        identification: [{ position: 100 }],
        fieldGroup: [{ qualifier: 'Technical', position: 60 }]
      }
      @EndUserText.label: 'FPM Extended'
      FpmExtended       : abap.char(5);

      @UI: {
        identification: [{ position: 110 }]
      }
      @EndUserText.label: 'App Name'
      AppName           : abap.char(255);

}
