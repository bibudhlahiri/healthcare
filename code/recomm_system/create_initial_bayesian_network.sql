--Start from a randomly generated Bayesian network where there is no edge between the variables of the same type.

drop function if exists health_recomm.create_initial_bayesian_network();
CREATE FUNCTION health_recomm.create_initial_bayesian_network() RETURNS void AS $$ 

 declare
  n_features integer := 0;
 begin

  drop SEQUENCE if exists health_recomm.feature_id_seq;
  CREATE SEQUENCE health_recomm.feature_id_seq START 1;

  drop table if exists health_recomm.features;
  create table health_recomm.features as
    select a.feature_name, nextval('health_recomm.feature_id_seq') as feature_id
    from (select * from (select distinct 'diag_' || dgns_cd as feature_name from health_recomm.transformed_claim_diagnosis_codes
          order by feature_name) b
          union all 
          select * from (select distinct 'proc_' || prcdr_cd as feature_name from health_recomm.transformed_claim_prcdr_codes
          order by feature_name) c
          union all 
          select * from (select distinct 'drug_' || substancename as feature_name from health_recomm.prescribed_drugs
          order by feature_name) d
          union all
          select * from (SELECT a.attname as feature_name
                         FROM pg_class c, pg_attribute a, pg_type t
                         WHERE c.relname = 'beneficiary_summary_2008_2009'
                         AND a.attnum > 0 AND a.attrelid = c.oid
                         AND a.atttypid = t.oid and a.attname <> 'desynpuf_id'
                         order by feature_name) e
          ) a;

   drop table if exists health_recomm.blacklist;
   create table health_recomm.blacklist as 
     (select a.feature_id as from_feature, b.feature_id as to_feature
      from health_recomm.features a, health_recomm.features b
      where substring(a.feature_name from 1 for 5) = 'diag_'
      and substring(b.feature_name from 1 for 5) = 'diag_')
     union all
     (select a.feature_id as from_feature, b.feature_id as to_feature
      from health_recomm.features a, health_recomm.features b
      where substring(a.feature_name from 1 for 5) = 'proc_'
      and substring(b.feature_name from 1 for 5) = 'proc_')
      union all
     (select a.feature_id as from_feature, b.feature_id as to_feature
      from health_recomm.features a, health_recomm.features b
      where substring(a.feature_name from 1 for 5) = 'drug_'
      and substring(b.feature_name from 1 for 5) = 'drug_')
      union all
     (select a.feature_id as from_feature, b.feature_id as to_feature
      from health_recomm.features a, health_recomm.features b
      where substring(a.feature_name from 1 for 6) = 'chron_'
      and substring(b.feature_name from 1 for 6) = 'chron_'
      and substring(a.feature_name from char_length(a.feature_name) - 3 for 4) = '2008'
       and substring(b.feature_name from char_length(b.feature_name) - 3 for 4) = '2008')
      union all
     (select a.feature_id as from_feature, b.feature_id as to_feature
      from health_recomm.features a, health_recomm.features b
      where substring(a.feature_name from 1 for 6) = 'chron_'
      and substring(b.feature_name from 1 for 6) = 'chron_'
      and substring(a.feature_name from char_length(a.feature_name) - 3 for 4) = '2009'
       and substring(b.feature_name from char_length(b.feature_name) - 3 for 4) = '2009');

    drop table if exists health_recomm.bayesian_network;
    select count(*) into n_features from health_recomm.features;

    drop SEQUENCE if exists health_recomm.edge_id_seq;
    CREATE SEQUENCE health_recomm.edge_id_seq START 1;
  
    --Create an initial bayesian_network by adding 5 outgoing edges to it at random
    create table health_recomm.bayesian_network as
      select nextval('health_recomm.edge_id_seq') as edge_id, feature_id as node_id, trunc(random()*n_features + 1) as neighbor_id
      from generate_series(1,5), health_recomm.features
      order by feature_id;

    --Remove the blacklisted ages that got created
    delete from health_recomm.bayesian_network f
    where exists (select 1 from health_recomm.blacklist b where b.from_feature = f.node_id and b.to_feature = f.neighbor_id);

    --Remove any edge that has a duplicate, i.e., connects the same pair of nodes
    delete from health_recomm.bayesian_network f
    where exists (select 1 from health_recomm.bayesian_network f1 where f.edge_id <> f1.edge_id and f.node_id = f1.node_id and 
                  f.neighbor_id = f1.neighbor_id);
 end;
$$ LANGUAGE plpgsql;

select * from health_recomm.create_initial_bayesian_network();

select * from health_recomm.bayesian_network;

select node_id, neighbor_id, count(*)
from health_recomm.bayesian_network
group by node_id, neighbor_id
having count(*) > 1

