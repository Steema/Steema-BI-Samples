{*********************************************}
{  TeeBI Software Library                     }
{  Constants in Spanish (Castilian) language  }
{  Copyright (c) 2016-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Languages.Spanish;

interface

procedure BISpanish;

implementation

uses
  BI.Languages.English;

procedure BISpanish;
begin
  BIMsg_About                       := 'Acerca de TeeBI...';
  BIMsg_Edit                        := 'Editar...';

  // DataItem
  BIMsg_DataItem_ChildNotFound      := 'Error: No existe el item: %s';
  BIMsg_CannotAccessData            := 'No es posible acceder al dato: %s desde: %s';
  BIMsg_RelativeOriginNoParent      := 'Error: No es posible acceder al origen relativo "..", los datos: %s no tienen propietario';
  BIMsg_UnNamed                     := '(sin nombre)';
  BIMsg_DuplicatedItem              := 'Error: Ya existe un item con el mismo nombre: %s';

  // Stores
  BIMsg_Store_ErrorOpening          := 'Error: %s abriendo almacen: %s';
  BIMsg_Store_NotRegistered         := 'Error: El almacen %s no está configurado en éste equipo';
  BIMsg_Store_MissingImporter       := 'Error: No se encuentra la clase de importación para el almacen: %s';
  BIMsg_Store_AlreadyExists         := 'Error: El almacen: %s ya existe';
  BIMsg_Store_DataLoadError         := 'Error: Cargando datos: %s del almacen: %s';
  BIMsg_Store_WrongName             := 'Error: El nombre de almacen: %s no puede contener el caracter ":"';
  BIMsg_Store_EmptyName             := 'Error: El nombre de almacen no puede ser vacio';
  BIMsg_Store_SelectRemote          := 'Selecciona un almacen remoto';
  BIMsg_Store_SureRemove            := '¿Estás seguro de eliminar el almacen: %s del registro?';
  BIMsg_Store_ChangeName            := 'Cambiar el nombre de almacen';
  BIMsg_Store_SureToDelete          := '¿Estás seguro de eliminar los datos: %s?';

  // Data Manager
  BIMsg_NewDataSource               := 'Nuevo Origen de Datos';
  BIMsg_Name                        := 'Nombre';
  BIMsg_Data_ChangeName             := 'Cambiar el nombre de los datos';
  BIMsg_DataSourceAlreadyExists     := 'Ya existen datos con el nombre: %s. Introduce un nombre diferente';

  // Web, Web Server
  BIMsg_Web_LatestVersionDownloaded := 'Última versión descargada. Reinicia por favor';
  BIMsg_Web_ErrorCheckingUpdates    := 'No es posible comprobar actualizaciones, error comprobando la última versión: %s';
  BIMsg_Web_NoDefaultStore          := 'El servidor BIWeb no se puede iniciar si ningún almacen por defecto';
  BIMsg_Web_ErrorWrongOrigin        := 'Error: Origen web incorrecto: %s';
  BIMsg_Web_ErrorEmptyDataStream    := 'Error: El paquete web de datos está vacio';
  BIMsg_Web_ErrorUnzip              := 'Error: Datos vacios descomprimiendo el paquete BIWeb';

  // Persist
  BIMsg_Persist_IncompatibleVersion := 'No es posible cargar los datos. Han sido creados con una versión antigua incompatible: %d';
  BIMsg_Persist_CorruptIndex        := 'Error: Datos corruptos, contador incorrecto: %s %d';
  BIMsg_Persist_CorruptLoad         := 'Error: Datos corruptos, tipo de datos incorrecto: %s (%s)';

  // Dataset
  BIMsg_Dataset_NoData              := 'No es posible abrir el BIDataSet. No hay datos asignados';
  BIMsg_Dataset_NoDetail            := 'Error: No es posible enlazar el BIDataset al maestro, no hay datos de detalle';
  BIMsg_Dataset_NotAsTable          := 'Error interno: Los datos no son "AsTable", buscando: %s';
  BIMsg_Dataset_DataNotFound        := 'Error interno: Datos no encontrados: %s';

  // Summary:
  BIMsg_Summary_NoMainData           := 'Sumarización: No hay ningun dato principal';
  BIMsg_Summary_AtLeastOne           := 'Sumarización: Se debe seleccionar al menos una Medida o un Grupo';
  BIMsg_Summary_ErrorSortParameter   := 'Error en el parámetro de ordenación: %s';
  BIMsg_Summary_WrongAggregate       := 'Error: Parámetro de agregación de medida incorrecto: %s';
  BIMsg_Summary_MeasureNilData       := 'Sumarización, no hay datos al añadir la Medida';
  BIMsg_Summary_MeasureNilExpression := 'Sumarización, no hay expresión al añadir la Medida';
  BIMsg_Summary_GroupByNilData       := 'Sumarización, no hay datos al añadir el Grupo';
  BIMsg_Summary_GroupByNilExpression := 'Sumarización, no hay expresión al añadir el Grupo';
  BIMsg_Summary_DeleteMeasure        := 'Error: Índice fuera de rango al eliminar la medida: %d';
  BIMsg_Summary_SwapMeasure          := 'Error: Índice fuera de rango al intercambiar la medida: %d';
  BIMsg_Summary_DeleteGroupBy        := 'Error: Índice fuera de rango al eliminar el Grupo: %d';
  BIMsg_Summary_SwapGroupBy          := 'Error: Índice fuera de rango al intercambiar el grupo: %d';
  BIMsg_Summary_MeasureName          := '%s de %s';

  // Arrays:
  BIMsg_DifferentArrayLength         := 'Las matrices (arreglos) son de longitud diferente';

  // JSON
  BIMsg_JSON_WrongClass              := 'Clase JSON incorrecta: %s';

  // XML
  BIMsg_XML_WrongContent             := 'Contenido XML incorrecto';

  // CSV
  BIMsg_CSV_FirstRowNoFields         := 'La primera fila no contiene ningún campo';

  // ZIP
  BIMsg_ZIP_FileNotValid             := 'El formato de archivo Zip no es válido: %s';

  // Import
  BIMsg_ImporterMissing              := 'No se encuentra la clase para importar los datos en: %s';
  BIMsg_FileImporterMissing          := 'No se encuentra la clase para importar los datos del archivo: %s';
  BIMsg_ImportError                  := 'Error al importar datos: %s';

  // Export
  BIMsg_Export                       := 'Exportar';
  BIMsg_Export_EmptyData             := 'Error: No es posible exportar datos vacios';

  // DB
  BIMsg_DB_NoEngineConfigured        := 'Error: No hay ningún motor de Base de Datos configurado. Usa la unidad BI.DB (Fire,SQLExpr,microOLAP,UniDAC,etc)';
  BIMsg_DB_SqlExpressWrongConnection := 'Error: La conexión no es de la clase TSQLConnection (motor SqlExpress)';
  BIMsg_ConnectionTestPassed         := 'Test de conexión correcto';

  // Http
  BIMsg_Http_UserCancelled           := 'La petición Http ha sido cancelada por el usuario';

  // Grid
  BIMsg_Grid_MissingEngine           := 'Error: No hay ningún "plugin" configurado para la BIGrid';

  // UI
  BIMsg_ChooseFolder                 := 'Selecciona una carpeta';
  BIMsg_NewName                      := 'Nuevo nombre:';
  BIMsg_Default                      := '(por defecto)';

  // Navigator
  BIMsg_NavigatorSingleItem          := '%d de %d';
  BIMsg_NavigatorItem                := '%d-%d de %d';
  BIMsg_NavigatorPage                := 'Página %d de %d';

  // Expressions
  BIMsg_ExpressionError              := 'Expresión incorrecta: %s en la posición: %d';
  BIMsg_ExpressionEmpty              := 'Error: Expresión vacia: %s';
  BIMsg_ExpressionFunctionMissing    := 'No existe la función: %s';
  BIMsg_ExpressionNotLogical         := 'La expresión: %s no es lógica (Sí/No)';

  // Sort
  BIMsg_SortNoItems                  := 'Error al ordenar: Es necesario definir al menos un Item';
  BIMsg_SortNoData                   := 'Error al ordenar: No se ha especificado ningún dato';
  BIMsg_SortWrongParent              := 'Error al ordenar: El propietario del dato: %s no es el dato: %s';

  // Server
  BIMsg_ServerAlreadyRunning         := 'El servidor BIWeb ya está en ejecución';
  BIMsg_ServerSureToClose            := '¿Seguro de cerrar éste servidor? (Hay %d conexiones activas)';

  // Misc
  BIMsg_Enabled                      := 'Activo';

  // BIVisual Dashboard Template
  BIMsg_LoadTemplate                 := 'Cargar plantilla';
  BIMsg_UnknownData                  := 'Error Datos desconocidos: %s';

  // Items Editor
  BIMsg_Field                        := 'Campo';

  // Native
  BIMsg_NativeFiles                  := 'TeeBI nativo';

  // Structure
  BIMsg_NotCompatible                := 'Error: Estructuras incompatibles: %s %s';

  // Algorithm Needs
  BIMsg_AlgorithmNeedQuantity        := 'Cantidad de datos incorrecta. Esperada: %d, Actual: %d';
  BIMsg_AlgorithmNeedKind            := 'Tipo de datos incorrecto: %s';
end;

end.
