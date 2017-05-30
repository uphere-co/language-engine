## Wiki named entity annotator
```
cabal configure --enable-tests
cabal test
# or run tests in parallel 
cabal build test
./dist/build/test/test  +RTS -N -RTS
#Or run tests in REPL
cabal repl test

#Clean up
cabal clean
```

## Haskell library on ETL tasks for CoreNLP
### Run tests in nix-shell
```
nix-shell shell.nix -I nixpkgs=~/repo/srcc/nixpkgs
cd tests
ghc -Wall corenlp_json.hs
./corenlp_json
```
## Wikidata ETL
- `rnn++/app/word_count` : Extract item from JSON dump.

Usages:
```
cat ~/word2vec/wikidata-20170206-all.json | ./wikidata_etl >wikidata.items

#Entity count by their properties
cat wikidata.items | awk -F $'\t' 'NF==5{print $3}' > wikidata.items.P31
cat wikidata.items | awk -F $'\t' 'NF==5{print $4}' > wikidata.items.P279

cat wikidata.items.P31 | tr ' ' '_' | ./word_count > wikidata.items.P31.count
cat wikidata.items.P279 | tr ' ' '_' | ./word_count > wikidata.items.P279.count

#Group items in the second column by the first column.
cat wikidata.items | awk 'BEGIN {FS="\t"};NF==5{print $3 "\t" $1}'> items.uid
./count > items.by_p31

#Print aliase entities only:
cat wikidata.items | awk 'BEGIN {FS="\t"};NF==2{print}' |head
#Get single word name entities
cat ~/word2vec/wikidata-20170206-all.json | ./wikidata_etl > wikidata.items
cat wikidata.items | awk 'BEGIN {FS="\t"};{print $1, $NF}' > wikidata.labels
cat wikidata.labels | java edu.stanford.nlp.process.PTBTokenizer -preserveLines > wikidata.labels.ptb
cat wikidata.labels.ptb | awk 'NF==2{print}' > wikidata.single_word
```

## Run CoreNLP Named Entity tagger
1. Download a zip file from [official site](http://nlp.stanford.edu/software/CRF-NER.shtml)
- Unzip the zip to `$CORENLP`
- Update `$CLASSPATH`
- Run following:
```
#use nlp-prototype/rnn++/app/wikidata_etl
cat ~/word2vec/wikidata-20170206-all.json | ./wikidata_etl >wikidata.items
cat wikidata.items | awk 'BEGIN {FS="\t"};{print $NF}' > wikidata.names
cat wikidata.names | awk 'NF==1{print}' > wikidata.names.single_word
java -mx48g edu.stanford.nlp.ie.NERClassifierCombiner -ner.model $CORENLP/classifiers/english.all.3class.distsim.crf.ser.gz,$CORENLP/classifiers/english.conll.4class.distsim.crf.ser.gz,$CORENLP/classifiers/english.muc.7class.distsim.crf.ser.gz -textFile wikidata.names.single_word > wikidata.names.single_word.ner
```
### Parse output of CoreNLP NER
```
cat wikidata.items | awk 'BEGIN {FS="\t"};{print "WIKIDATAITEM_" $1 "\t" $NF}' > wikidata.ner_input 
java -mx48g edu.stanford.nlp.ie.NERClassifierCombiner -ner.model $CORENLP/classifiers/english.all.3class.distsim.crf.ser.gz,$CORENLP/classifiers/english.conll.4class.distsim.crf.ser.gz,$CORENLP/classifiers/english.muc.7class.distsim.crf.ser.gz -textFile wikidata.ner_input > wikidata.ner
#Use corenlp/wiki/corenlp_ner.hs
```

# Filter named entities from Wikidata entities
```
#Input : "wikidata.ner"
./corenlp_ner  > wikidata.is_sfne
cat wikidata.is_sfne | awk 'BEGIN {FS="\t"};{print $1 "\t" $NF}' > wikidata.uid.is_sfne

#Input : "items.by_p31", "wikidata.uid.is_sfne"
./ne_by_property > wikidata.p31.is_ne

cat wikidata.items | awk 'BEGIN {FS="\t"};NF==5{print $1 "\t" $3  "\t" $NF}' > wikidata.names
#Input : "wikidata.names", "wikidata.p31.is_ne"
./wikidata_ner > wikidata.nes
```

