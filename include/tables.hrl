-record(tname, {field1, field2}).

-define(TABLES, [{table_name, 
                  [{access_mode, read_write},
                   {attributes, record_info(fields, tname)},
                   {type, set}]}
                ]).
