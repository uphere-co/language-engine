#### Output file schemes
##### category_hierarchy 
|sub_cat_id | super_cat | sub_cat
|------|------|------
|1000014|Food_preservation|Curing_agents

##### page_id.category.wikidata.sorted
|page_id | category | page_title | page_wiki_uid
|------|------|------|------
|1000|Fictional_Belgian_people|Hercule_Poirot|Q170534

#### ETL steps
##### Nix-shell setup
Current ETL process depends on a python script, `mysqldump_to_csv.py`, in https://github.com/jamesmishra/mysqldump-to-csv
```
nix-shell shell-wiki.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20
```
##### Convert SQL dump files to TSC files
```
$ time pigz -dc enwiki-latest-categorylinks.sql.gz | iconv -f ISO-8859-1 -t UTF-8 | python mysqldump_to_csv.py |tr -d '\r' | lbzip2 --fast > enwiki-latest-categorylinks.tsv.bz2
real	18m58.210s
$ time pigz -dc enwiki-latest-category.sql.gz | iconv -f ISO-8859-1 -t UTF-8 | python mysqldump_to_csv.py |tr -d '\r'  | lbzip2 --fast > enwiki-latest-category.tsv.bz2
real	0m7.684s
$ time pigz -dc enwiki-latest-page_props.sql.gz | iconv -f ISO-8859-1 -t UTF-8 | python mysqldump_to_csv.py |tr -d '\r'  | lbzip2 --fast > enwiki-latest-page_props.tsv.bz2
real	1m43.029s
$ time pigz -dc enwiki-latest-page.sql.gz | iconv -f ISO-8859-1 -t UTF-8 | python mysqldump_to_csv.py |tr -d '\r'  | lbzip2 --fast > enwiki-latest-page.tsv.bz2
real	7m39.601s
```

##### ETL to get category hierarchy
```
#Get links from category pages.
time lbzcat enwiki-latest-categorylinks.tsv.bz2| awk -F"\t" '$NF=="subcat"{print $1 "\t" $2}' |sort -k1,1 -t$'\t' > enwiki-latest-categorylinks.category.sorted
real	5m4.965s
#Get page_id of category pages.
$ time lbzcat enwiki-latest-page.tsv.bz2 | awk -F "\t" '$2==14{print $1 "\t" $3 }' | sort -k1,1 -t$'\t' > enwiki-latest-page.category.sorted
real	0m28.621s
#Get links between category pages
$ time join -t$'\t' enwiki-latest-categorylinks.category.sorted enwiki-latest-page.category.sorted > category_hierarchy
real	0m7.733s
```

##### ETL to get pages in a category
```
#Get cat_id of category pages.
$ time lbzcat enwiki-latest-category.tsv.bz2 | awk -F "\t" '{print $1 "\t" $2}'| sort -k1,1 -t$'\t'  > cat_id
real	0m3.890s
#Get page_id of Wikipedia pages
$ time lbzcat enwiki-latest-page.tsv.bz2 | awk -F "\t" '$2==0{print $1 "\t" $3 }' | sort -k1,1 -t$'\t' > page_id
user	3m4.550s
#Get Wikidata ID of Wikipedia pages
$ time lbzcat enwiki-latest-page_props.tsv.bz2 | awk -F "\t" '$2=="wikibase_item"{print $1 "\t" $3}' | sort -k1,1 -t$'\t' > page_id.wiki_id.sorted
real	0m31.482s
#Get Wikipedia UID for Wikipedia page titles
$ time join -t$'\t' page_id page_id.wiki_id.sorted > page_id.wiki_id.txt.sorted
real	0m8.493s

#Get links from category pages.
$ time lbzcat enwiki-latest-categorylinks.tsv.bz2| awk -F"\t" '$NF=="page"{print $1 "\t" $2}' |sort -k1,1 -t$'\t' > enwiki-latest-categorylinks.page.sorted
real	9m9.721s
#Get categories per page_id(and its title and Wikidata UID)
$ time join -t$'\t' enwiki-latest-categorylinks.page.sorted page_id | join -t$'\t' - page_id.wiki_id.sorted > page_id.category.wikidata.sorted
real	0m53.164s
```
