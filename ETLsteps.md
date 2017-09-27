Generate a new dataset from scratch, from Wikipedia and Wikidata dump.

### List of all dataset
ne.org, ne.person, ne.brand, ne.loc, ne.occupation, ne.human_rule, ne.building
 - a list of Wikidata UID of a given type.
names
 - names and alias per Wikidata UID
page_id.wiki_id.wordnet.tsv, interlinks.filtered, wiki_id.page_title.txt
 - used for disambiguation
terms
 - Investopedia terms
 - Check T549 to generate it

unnecessary data(used in past tests):
properties.tsv
companies


### Generate Wikidata dataset.
Input  : wikidata-20170904-all.json.bz2
Output : wikidata.all_entities, wikidata.items

Check "Prepare data and run the executable for entity linking" section of `wiki-ner/README.md`

```
# Use shell.nix of nlp-prototype/rnn++ with wavewave's fork of nixpkgs
lbzcat /scratch/groups/uphere/wikidata/wikidata-20170904-all.json.bz2 | ./wikidata_etl /opt/develset.rss/config.rss.json | grep -v "^P" > wikidata.items
cat wikidata.items | awk -F '\t' '{print $1 "\t" $NF}' > wikidata.all_entities
```


### Generate data from DBpedia
Check "ETL to get Wikidata - WordNet mapping" section of `wiki-ner/scripts/WikipediaETL.md`
and download `wordnet_links.ttl`

### Generate data from Wikipedia dump
Check `wiki-ner/scripts/WikipediaETL.md`

Download necessary sql.gz files from Wikipedia dump download page.

Input : *.sql.gz, mysqldump_to_csv.py
Output : *.tsv.bz2

Input : enwiki-latest-page.tsv.bz2
Output : page_id

Input : enwiki-latest-page_props.tsv.bz2
Output : page_id.wiki_id.sorted

Input : page_id, page_id.wiki_id.sorted
Output : page_id.wiki_id.txt.sorted

Input : dbpedia/wordnet_links.ttl, page_id.wiki_id.txt.sorted
Output : page_id.wiki_id.wordnet.tsv

Input : page_id.wiki_id.txt.sorted
Output: wiki_id.page_title.txt


### Generate Wikipedia alias dataset
Check `wiki-ner/scripts/WikipediaETL.md`
Download yagoTaxonomy.tsv.7z from the YAGO site.

Input : yagoTaxonomy.tsv, Using `yago-bin`, print `typedCats` from `wikicatOfWordNetT`
Output : typedCats


Check "ETL to get name alias for named entities" section of `wiki-ner/scripts/WikipediaETL.md`
Input : enwiki-20170620-redirect.tsv.bz2
Output : redirects

Input  : enwiki-latest-categorylinks.tsv.bz2
Output : enwiki-latest-categorylinks.page.sorted
real	9m9.869s

Input  : enwiki-latest-categorylinks.page.sorted, page_id, page_id.wiki_id.sorted
Output : page_id.category.wikidata.sorted
real	0m38.798s

Input  : yago/typedCats, page_id.category.wikidata.sorted
Output : page_id.category.wikidata.wordnet.sorted
real	1m0.417s

Input  : redirects, page_id.category.wikidata.wordnet.sorted, page_id.wiki_id.txt.sorted
Output : names.redirect

### Generate per entity type data.
Input : wikidata.all_entities, wikidata.items, names.redirect
Output : ne.*
Check `wiki-ner/scripts/gen_data.sh`

```
cd ~/repo/uphere/wiki-ner
nix-shell shell.nix --arg pkgs "import $HOME/repo/srcc/nixpkgs {}" --max-jobs 20 --cores 20
# move to a new directory
mkdir newdata
cd newdata
# Get csv files from Wikidata.
../scripts/wikidata_queries.sh
```

Input : names.redirect, ne.*
Output : names
Check `wiki-ner/scripts/gen_data.sh`




### Generate dataset for disambiguation
For details, see `wiki-ner/README.md`

Input  : yago/yago3_entire_tsv.bz2, Using `yago-bin`, print `links` from `interWikiLinks`
Output : interlinks

```
$ time lbzcat ../yago/yago3_entire_tsv.bz2 | grep "<linksTo>" > wikilinks
real	3m27.965s
$ time cat wikilinks |  ../dist/build/yago-bin/yago-bin > interlinks
real	3m0.886s
$ time cat interlinks  | awk -F '\t' '{print $1 "\n" $2}' > nodes.weighted
real	0m47.505s
```
Input  : interlinks
Output : nodes.weighted.ran

Input  : interlinks, nodes.weighted.ran,  `node-filter` binary
Output : filter.page
$ time ../dist/build/node-filter/node-filter > filter.page
real	41m12.292s

Input : filter.page, interlinks
Output : interlinks.filtered


