## Run CoreNLP NER
```
# for word tokenization
java edu.stanford.nlp.process.PTBTokenizer -preserveLinesjava edu.stanford.nlp.process.PTBTokenizer -preserveLines data/bloomberg2.txt > data/bloomberg2.ptb 
# for NER
java -mx48g edu.stanford.nlp.ie.NERClassifierCombiner -ner.model $CORENLP/classifiers/english.all.3class.distsim.crf.ser.gz,$CORENLP/classifiers/english.conll.4class.distsim.crf.ser.gz,$CORENLP/classifiers/english.muc.7class.distsim.crf.ser.gz -textFile data/bloomberg2.txt > data/bloomberg2.ner
# for POS tagging : Check HCoreNLP/README.md 
```
## Output file schemes
#### category_hierarchy 
|sub_cat_id | super_cat | sub_cat
|------|------|------
|1000014|Food_preservation|Curing_agents

#### page_id.category.wikidata.sorted
|page_id | category | page_title | page_wiki_uid
|------|------|------|------
|1000|Fictional_Belgian_people|Hercule_Poirot|Q170534

#### redirects.sorted
|title_from | title_to
|------|------
|3M_Company | 3M

#### jel.category
Contains JEL code for Wikipedia categories
|cat_name | JEL code | JEL major code
|------|------|------
|Joint_ventures | L24 | L

#### jel.wikidata 
Contains JEL code for Wikipedia pages
| page_id | cat_name |  JEL code | JEL major code | page_name | page_wikidata
|------|------|------|------|------|------
|10000895 | IT_risk_management | M15 | M | Incident_response_team | Q349987

#### companies 
Contains GICS and ticker symbol for companies (presently, S&P 500 and S&P 400)
|page_title | ticker | GICSsector | GICSsubsector | page_id | page_wiki_uid
|-------|-------|-------|-------|-------|-------
| Abiomed | ABMD | Health Care | Health Care Equipment | 6872689 | Q4667884

#### WordNet mapping
|page_title| page_id | page_wiki_uid | wordnet
|-------|-------|-------|-------
|Google | 1092923 | Q95 | synset-company-noun-1

## ETL steps
#### Nix-shell setup
Current ETL process depends on a python script, `mysqldump_to_csv.py`, in https://github.com/jamesmishra/mysqldump-to-csv
```
nix-shell shell-wiki.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20
```
#### Convert SQL dump files to TSV files
```
# Use wiki-ner/shell-wiki.nix
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

#### ETL to get category hierarchy
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

#### ETL to get pages in a category
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
$ time lbzcat enwiki-latest-categorylinks.tsv.bz2 | awk -F"\t" '$NF=="page"{print $1 "\t" $2}' |sort -k1,1 -t$'\t' > enwiki-latest-categorylinks.page.sorted
real	9m9.721s
#Get categories per page_id(and its title and Wikidata UID)
$ time join -t$'\t' enwiki-latest-categorylinks.page.sorted page_id | join -t$'\t' - page_id.wiki_id.sorted > page_id.category.wikidata.sorted
real	0m53.164s
```

#### Get data from Wikipedia tables
Current list of tables :
1. S&P 500 companies 
- S&P 400 companies

```
nix-shell shell-wiki.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20
# Move to the data directory
cd /scratch/groups/uphere/enwiki/
python ~/repo/uphere/wiki-ner/scripts/get_wikipedia_tables.py
```

#### ETL to get companies with its ticker symbol, GICS Sector, and GICS Sub Industry.
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

#### ETL to get JEL code for Wikipedia categories
First of all, Export `Category:Categories_which_are_included_in_the_JEL_classification_codes` category via [Wikipedia Special:Export](https://en.wikipedia.org/wiki/Special:Export).
Presently, it is stored as `mark:/scratch/groups/uphere/enwiki/Wikipedia-20170615063859.xml`.
```
cd /scratch/groups/uphere/enwiki/
python ~/repo/uphere/wiki-ner/scripts/get_jel_codes.py
```

#### ETL to get JEL code for Wikidata items
```
# Join by category name
join -1 2 -2 1 -t$'\t' <(sort -t$'\t' -k2,2 category_hierarchy) <(sort -t$'\t' -k1,1 jel.tsv) | awk -F '\t' '{print $3 "\t" $6 "\t" $7}' > jel.category
cat jel.tsv | awk -F '\t' '{print $1 "\t" $4 "\t" $5}' >> jel.category


# Join by category name
$ time join -1 2 -2 1 -t$'\t' <(sort -t$'\t' -k2,2 enwiki-latest-categorylinks.page.sorted) <(sort -t$'\t' -k1,1 jel.category) > jel.page
real	2m17.101s

# Join by page_id
join -1 2 -2 1 -t$'\t' <(sort -t$'\t' -k2,2 jel.page) page_id.wiki_id.txt.sorted > jel.wikidata
```

#### ETL to get Wikidata - Wikipedia title mapping
```
cat enwiki/page_id.wiki_id.txt.sorted | awk -F '\t' '{print $3 "\t" $2}' > data/wiki_id.page_title.txt
```

#### ETL to get Wikidata - WordNet mapping 
Download `wordnet_links.ttl.gz` from [here](http://downloads.dbpedia.org/2016-04/links/).
```
tail +2 dbpedia/wordnet_links.ttl | sed 's/> / /g' | awk '/^#/ {next} {print $1 "\t" $3}' | sed 's/<http:\/\/dbpedia.org\/resource\///g' | sed 's/<http:\/\/www.w3.org\/2006\/03\/wn\/wn20\/instances\///g'  > wordnet_links.tsv
join -1 2 -2 1 -t$'\t' <(sort -k2,2 -t$'\t' page_id.wiki_id.txt.sorted) <(sort -k1,1 -t$'\t' wordnet_links.tsv) > page_id.wiki_id.wordnet.tsv
```

### ETL to get name alias for named entities
```
$ time lbzcat wikidata-20170627-truthy-BETA.nt.bz2 | grep "@en " | grep "<http://www.w3.org/2004/02/skos/core" | lbzip2 --fast > wikidata-20170627-truthy-BETA.nt.names.bz2
real	5m18.808s

$ time lbzcat wikidata-20170627-truthy-BETA.nt.bz2 | grep "@en " | grep "<http://www.w3.org/2004/02/skos/core" > wikidata-20170627-truthy-BETA.nt.names
real	5m3.000s


$ time join -j 1 -t$'\t' <(lbzcat enwiki-latest-redirect.tsv.bz2 | awk -F "\t" '{print $1 "\t" $3}'| sort -k1,1 -t$'\t') <(sort -k1,1 -t$'\t' page_id) > redirects
real	0m24.686s

$ time join -j 2 -t$'\t' <(sort -k2,2 -t$'\t' page_id.wiki_id.txt.sorted) <(sort -k2,2 -t$'\t' redirects) > page_title.page_id.wiki_id.redirect
real	0m13.916s


$ time cat page_title.page_id.wiki_id.redirect |tr "_" " " | awk -F "\t" '{print "<http://www.wikidata.org/entity/" $3 "> <http://uphere.ai/v0/altLabel#Wikipedia_redirect> \"" $5"\"@en"}' > item_names.wikipedia_redirect.nt
real	0m13.941s


$ time cat page_id.wiki_id.txt.sorted |tr "_" " " | awk -F "\t" '{print "<http://www.wikidata.org/entity/" $3 "> <http://uphere.ai/v0/altLabel#Wikipedia_title> \"" $2"\"@en"}' > item_names.wikipedia_title.nt
real	0m7.837s

cp ../wikidata/wikidata-20170627-truthy-BETA.nt.names.bz2 item_names.nt.bz2
cat item_names.wikipedia_*.nt | lbzip2 >> item_names.nt.bz2
```


## YAGO

```
# no multi-threaded 7z (de)compressor in linux; replace it with bz2.
$ time 7za e -so yago3_entire_tsv.7z | lbzip2 --fast > yago3_entire_tsv.bz2
real	17m40.827s
# drop triples for non-english Wikipedia entities
$ time lbzcat yago3_entire_tsv.bz2 | grep -v '<[a-z][a-z]/' | lbzip2 --fast > yago3_entire.en.tsv.bz2
real	15m55.255s
-rw-r--r-- 1 jihuni uphere 11445536941 Jul  5 07:10 yago3_entire.en.tsv.bz2
-rw-r--r-- 1 jihuni uphere 13613624731 Jul  5 01:51 yago3_entire_tsv.bz2
```

## YAGO and Wikidata
#### Get WordNet synset per entity
```
cabal build yago-bin
# run with `wikicatOfWordNetT`:
$ cat yago/yagoTaxonomy.tsv | dist/build/yago-bin/yago-bin typedCat > typedCats 
$ time join -1 2 -2 1 -t$'\t' <(sort -k2,2 -t$'\t' enwiki/page_id.category.wikidata.sorted) <(sort -k1,1 -t$'\t' yago/typedCats) > enwiki/page_id.category.wikidata.wordnet.sorted
real	1m5.242s
```

#### Get names per entity from Wikipedia redirects
```
$ time cat page_id.category.wikidata.wordnet.sorted | awk -F '\t' '{print $3}' | sort | uniq > entities.wiki
real	0m52.277s
$ time cat redirects | grep -Fwf entities.wiki > entities.wiki_alias
real	0m31.076s
$ time join -1 2 -2 2 -t$'\t' <(sort -k2,2 -t$'\t' entities.wiki_alias)  <(sort -k2,2 -t$'\t' page_id.wiki_id.txt.sorted) | sed 's/_/ /g' | awk -F '\t' '{print $5 "\t" $3}' > names.redirect
real	0m14.269s
```

