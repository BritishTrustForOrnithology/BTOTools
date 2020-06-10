select b.master_taxon_id,b.scientific_name,nvl(lsa.bou_name,b.english_name) ENGLISH_NAME,
b.taxon_rank_id,ltr.TAXON_RANK_NAME, b.sort_order,
case when lsa.bou_name is not null then b.english_name else null end ALT_INT_NAME,
lsa.CBC_CODE,lsa.FIVE_LETTER,lsa.euring_no
from species.baselist_taxa b left join species.l_species_aux lsa on b.master_taxon_id=lsa.master_taxon_id
inner join L_TAXON_RANK ltr on ltr.TAXON_RANK_ID=b.TAXON_RANK_ID
where b.baselist_abbrev='IOC_BTO'
and b.baselist_version=10.1
order by b.sort_order
;

