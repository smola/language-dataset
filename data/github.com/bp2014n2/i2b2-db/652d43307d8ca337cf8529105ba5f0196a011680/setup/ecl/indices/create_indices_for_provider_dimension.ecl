IMPORT STD;
provider_dimension_record := RECORD
  string50 provider_id;
  string700 provider_path;
  string850 name_char;
  string provider_blob;
  string25 update_date;
  string25 download_date;
  string25 import_date;
  string50 sourcesystem_cd;
  integer5 upload_id;
 END;

provider_dimension := DATASET('~i2b2demodata::provider_dimension', provider_dimension_record, FLAT);

provider_dimension_idx_all_queried := INDEX(provider_dimension, {provider_path}, {provider_id}, '~i2b2demodata::provider_dimension_idx_all_queried');
BUILD(provider_dimension_idx_all_queried, SORT ALL, OVERWRITE);

description := 'XDBC:RelIndexes=[i2b2demodata::provider_dimension_idx_all_queried]';
STD.File.SetFileDescription('~i2b2demodata::provider_dimension',description);