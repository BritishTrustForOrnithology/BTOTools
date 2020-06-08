select distinct
l_ioc.master_taxon_id,
b.scientific_name ALTERNATIVE_SCI_NAME,
b.taxon_rank_id ALTERNATIVE_RANK_ID,
ltr2.taxon_rank_name ALTERNATIVE_RANK_NAME,
l_ioc.scientific_name CURRENT_SCI_NAME,
l_ioc.taxon_rank_id CURRENT_RANK_ID,
ltr1.taxon_rank_name CURRENT_RANK_NAME,
l_ioc.sort_order SORT_ORDER_IOC
,COUNT(*) NUMBER_OF_LISTS
from baselist_taxa b 
inner join 
(select master_taxon_id,scientific_name,sort_order,taxon_rank_id from baselist_taxa where baselist_abbrev='IOC_BTO' and baselist_version=10.1) l_ioc
on b.master_taxon_id=l_ioc.master_taxon_id
inner join l_taxon_rank ltr1 on ltr1.taxon_rank_id=l_ioc.taxon_rank_id
inner join l_taxon_rank ltr2 on ltr2.taxon_rank_id=b.taxon_rank_id
where 1=1
and b.scientific_name<>l_ioc.scientific_name
group by l_ioc.master_taxon_id,b.scientific_name,b.taxon_rank_id,ltr2.taxon_rank_name,l_ioc.scientific_name,l_ioc.taxon_rank_id,ltr1.taxon_rank_name,l_ioc.sort_order
order by l_ioc.sort_order
;