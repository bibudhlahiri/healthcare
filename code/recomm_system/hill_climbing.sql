﻿--Apply hill climbing to maximize the BIC score over the network
drop function if exists health_recomm.create_all_configs_view(bigint);
create function health_recomm.create_all_configs_view(bigint) RETURNS void AS $$ 

  declare
   v_feature_id alias for $1;
   create_all_configs varchar(3000) := '';
   
   curParents CURSOR (child_id integer) FOR 
     select f.feature_id, f.feature_name 
     from health_recomm.bayesian_network bn, health_recomm.features f
     where bn.node_id = f.feature_id
     and bn.neighbor_id = child_id;

  begin
  --Get the parents of this feature in the current Bayesian network
   create_all_configs := 'drop view if exists health_recomm.configurations; ';
   create_all_configs := create_all_configs || 'create view health_recomm.configurations as select * from generate_series(0,1) feature_' 
                         || v_feature_id || ',';
   for rec_parent in curParents(v_feature_id) loop
    --Create the SQL query dynamically since the number of clauses to find N_i_j_k depends on no. of parents.
    --First, create a temporary table with all possible configurations combining the features and its parents.
     create_all_configs := create_all_configs || 'generate_series(0,1) feature_' || rec_parent.feature_id || ','; 
   end loop;
   create_all_configs := trim(trailing ',' from create_all_configs);
   raise notice 'feature_id = %, create_all_configs = %', v_feature_id, create_all_configs;
   execute create_all_configs;
  end;
$$ LANGUAGE plpgsql;


drop function if exists health_recomm.populate_N_i_j_k();
CREATE FUNCTION health_recomm.populate_N_i_j_k() RETURNS void AS $$ 

 declare
  n_features integer := 0;
  
  curFeatures cursor for 
     select *
     from health_recomm.features;
     
 begin
  --For each feature X_i, compute N_i_j_k, where x_{i1} = 1 and x_{i2} = 0 
  for rec_feature in curFeatures loop
    perform health_recomm.create_all_configs_view(rec_feature.feature_id);
  end loop;
 end;
$$ LANGUAGE plpgsql;

select * from health_recomm.populate_N_i_j_k();

