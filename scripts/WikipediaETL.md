#### Output file schemes
##### category_hierarchy 
|sub_cat_id | super_cat | sub_cat
|------|------|------
|1000014|Food_preservation|Curing_agents

##### page_id.category.wikidata.sorted
|page_id | category | page_title | page_wiki_uid
|------|------|------|------
|1000|Fictional_Belgian_people|Hercule_Poirot|Q170534

##### redirects.sorted
|title_from | title_to
|------|------
|3M_Company | 3M

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
$ time pigz -dc enwiki-latest-pagelinks.sql.gz | iconv -f ISO-8859-1 -t UTF-8 | python mysqldump_to_csv.py |tr -d '\r'  | lbzip2 --fast > enwiki-latest-pagelinks.tsv.bz2
real	66m30.858s
$ time pigz -dc enwiki-latest-redirect.sql.gz | iconv -f ISO-8859-1 -t UTF-8 | python mysqldump_to_csv.py |tr -d '\r'  | lbzip2 --fast > enwiki-latest-redirect.tsv.bz2
real	0m42.250s
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

##### Get data from Wikipedia tables
Current list of tables :
1. S&P 500 companies 
- S&P 400 companies

```
nix-shell shell-wiki.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20
# Move to the data directory
cd /scratch/groups/uphere/enwiki/
python ~/repo/uphere/wiki-ner/scripts/get_wikipedia_tables.py
```

##### ETL to get companies with its ticker symbol, GICS Sector, and GICS Sub Industry.
Run `get_wikipedia_tables.py` before doing this step.
```
$ time join -t$'\t' <(lbzcat enwiki-latest-redirect.tsv.bz2 | awk '{print $1 "\t" $3}'|sort -k1,1) page_id | awk '{print $3 "\t" $2}' | sort -k1,1 > redirects.sorted
real	0m50.731s

join -1 2 -2 2 -t$'\t' <(sort -k2,2 -t$'\t' sp500companies.tsv|awk -F '\t' '{print $1 "\t" $2 "\t" $4 "\t" $5}') <(sort -k2,2 -t$'\t' page_id.wiki_id.txt.sorted) > sp500.0 
join -t$'\t' -1 2 -2 1 <(sort -k2,2 -t$'\t' sp500companies.tsv) redirects.sorted  | awk -F '\t' '{print $2 "\t" $NF "\t" $4 "\t" $5}' > sp500.redirects
join -1 2 -2 2 -t$'\t' <(sort -k2,2 -t$'\t' sp500.redirects) <(sort -k2,2 -t$'\t' page_id.wiki_id.txt.sorted) > sp500.1

join -1 2 -2 2 -t$'\t' <(sort -k2,2 -t$'\t' sp400companies.tsv|awk -F '\t' '{print $1 "\t" $2 "\t" $3 "\t" $4}') <(sort -k2,2 -t$'\t' page_id.wiki_id.txt.sorted) > sp400.0 
join -t$'\t' -1 2 -2 1 <(sort -k2,2 -t$'\t' sp400companies.tsv) redirects.sorted  | awk -F '\t' '{print $2 "\t" $NF "\t" $3 "\t" $4}' > sp400.redirects
join -1 2 -2 2 -t$'\t' <(sort -k2,2 -t$'\t' sp400.redirects) <(sort -k2,2 -t$'\t' page_id.wiki_id.txt.sorted) > sp400.1
cat sp?00.? > companies
```

##### ETL to get JEL code for Wikipedia categories
First of all, Export `Category:Categories_which_are_included_in_the_JEL_classification_codes` category via [Wikipedia Special:Export](https://en.wikipedia.org/wiki/Special:Export).
Presently, it is stored as `mark:/scratch/groups/uphere/enwiki/Wikipedia-20170615063859.xml`.
```
cd /scratch/groups/uphere/enwiki/
python ~/repo/uphere/wiki-ner/scripts/get_jel_codes.py
```
