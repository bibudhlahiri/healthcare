drop function if exists health_recomm.convert_to_wide_format();
CREATE FUNCTION health_recomm.convert_to_wide_format() RETURNS void AS $$ 

 declare
   create_table_statement varchar(200000) := 'create table dense_matrix (desynpuf_id varchar(100), ';
   curFeatures cursor for 
     select *
     from health_recomm.features;
 begin
   for rec_feature in curFeatures loop
      create_table_statement := create_table_statement || rec_feature.feature_name || ' varchar(2),';
   end loop;
   --Trim last comma
   create_table_statement := trim(trailing ',' from create_table_statement);
   create_table_statement := create_table_statement || ')';
   execute create_table_statement;
 end;
$$ LANGUAGE plpgsql;

select * from health_recomm.convert_to_wide_format();
