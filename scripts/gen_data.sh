#Run CoreNLP
java edu.stanford.nlp.process.PTBTokenizer -preserveLines iphone.txt > iphone.ptb
java -mx48g edu.stanford.nlp.ie.NERClassifierCombiner -ner.model $CORENLP/classifiers/english.all.3class.distsim.crf.ser.gz,$CORENLP/classifiers/english.conll.4class.distsim.crf.ser.gz,$CORENLP/classifiers/english.muc.7class.distsim.crf.ser.gz -textFile iphone.ptb > iphone.ner

DIR=/scratch/groups/uphere/wikidata
tail -n +2 $DIR/public_companies.csv| awk -F ',' '{print $1}' | awk -F "/" '{print $NF}' > ne.company
tail -n +2 $DIR/businesspersons.csv | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}' > ne.business_person   
tail -n +2 $DIR/employees.csv       | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}' >> ne.business_person

QEDIR=/opt/develset.rss/
grep -Fwf ne.business_person /opt/develset.rss/wikidata.all_entities > uid.business_person
grep -Fwf ne.company /opt/develset.rss/wikidata.all_entities    > uid.company
grep -Fwf <(cat ne.*) /opt/develset.rss/wikidata.properties > property

tail -n +2 orgs.csv | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}'  > org_types
tail -n +2 persons.csv | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}'  > person_types
tail -n +2 occupations.csv | awk -F ',' '{print $1}' | awk -F "/" '{print $NF}'  > occupation_types

grep -Fwf org_types $DIR/wikidata.items | awk -F '\t' '{print $1}' > items.org
grep -Fwf person_types $DIR/wikidata.items | awk -F '\t' '{print $1}' > items.person
grep -Fwf occupation_types $DIR/wikidata.items | awk -F '\t' '{print $1}' > items.occupation

#To be updated.
#grep -Fwf items.org $DIR/wikidata.all_entities > ne.org
#grep -Fwf items.person $DIR/wikidata.all_entities > ne.person
#grep -Fwf items.occupation $DIR/wikidata.all_entities > ne.occupation

#Note : it takes ~ 3 mins
grep -Fwf ne.person $DIR/wikidata.all_entities > uid.person
#Note : it takes ~ 80 s
grep -Fwf ne.org $DIR/wikidata.all_entities    > uid.org


# Get type hierarchy for organization types
tail -n +2 orgs.csv | awk -F ',' '{n=split($1,a,"/"); print a[n] "\t" $2}' > org_types
tail -n +2 orgs.csv | awk -F ',' '{n=split($1,a,"/"); print a[n]}' > org_types.uid

# Get a list of Wikidata properties
tail -n +2 properties.csv | awk -F ',' '{n=split($1,a,"/"); print a[n] "\t" $2}' > properties.tsv

##takes ~22s
#awk -F"\t" 'NR == FNR { a[$1]; next } NF==5 && $1 in a {print}' org_types.uid wikidata.items > items.org
#awk -F"\t" 'NR == FNR { a[$1]=$2; next } NF==5 {split($4, p279s, " "); for(i in p279s) {p=p279s[i];if(p in a) print $1 "\t" $NF "\t" p "\t" a[p]}}' org_types items.org > types.org

# For Brand class
## Get type hierarchy for brand types
tail -n +2 brands.csv | awk -F ',' '{n=split($1,a,"/"); print a[n] "\t" $2}' > brand_types
tail -n +2 brands.csv | awk -F ',' '{n=split($1,a,"/"); print a[n]}' > brand_types.uid
# Get instance of brand class
## real	0m34.983s
awk -F"\t" 'NR == FNR { a[$1]=$2; next } NF==5 {split($3, p31s, " "); for(i in p31s) {p=p31s[i];if(p in a) print $1 "\t" $NF "\t" p "\t" a[p]}}' brand_types wikidata.items > items.brand
# Get subclass of brand class
## real	0m32.243s
awk -F"\t" 'NR == FNR { a[$1]=$2; next } NF==5 {split($4, p279s, " "); for(i in p279s) {p=p279s[i];if(p in a) print $1 "\t" $NF "\t" p "\t" a[p]}}' brand_types wikidata.items > types.brand
## Get UID list
awk '{print $1}' items.brand > ne.brand
## Get representations
grep -Fwf ne.brand ../wikidata/wikidata.all_entities > uid.brand

#Aggregate uid reprs.
cat uid.* | sort | uniq > uid


# Public companies with their P31 and P279 property values
grep -Fwf ne.company wikidata.items | awk -F"\t" 'NF==5{print}' > items.company
# Print company and their organization types
awk -F"\t" 'NR == FNR { a[$1]=$2; next } NF==5 {split($3, p31s, " "); for(i in p31s) {p31=p31s[i];if(p31 in a) print $1 "\t" $NF "\t" p31 "\t" a[p31]}}' org_types items.company > class.company


# Get lines that contain property values
# cd /scratch/groups/uphere/wikidata/
cat wikidata.items | awk -F"\t" 'NF==5{print}' > wikidata.items.properties


