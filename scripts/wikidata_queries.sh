#List persons work for company
curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT DISTINCT ?person ?personLabel ?firstName ?firstNameLabel ?lastName ?lastNameLabel ?company ?companyLabel WHERE {
   ?person  wdt:P108 ?company .
   optional{ ?person wdt:P735 ?firstName.}
   optional{ ?person wdt:P734 ?lastName.}
   ?company wdt:P31/wdt:P279* wd:Q4830453 .
   SERVICE wikibase:label {
    bd:serviceParam wikibase:language "en" .
   }
 }
' > employees.csv

#List persons related to public companies (e.g. founder, CEO, board members, and so on)
curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT DISTINCT ?person ?personLabel ?firstName ?firstNameLabel ?lastName ?lastNameLabel ?company ?companyLabel WHERE {
   ?company wdt:P414 ?excahge .
   ?company wdt:P31/wdt:P279* wd:Q43229 .
   ?company p:P1789|p:P3320|p:P1037|p:P169|p:P127|p:P112|p:P488 ?person_statement .
   ?person_statement ?property ?person .
   ?person wdt:P31 wd:Q5 .
   optional{ ?person wdt:P735 ?firstName.}
   optional{ ?person wdt:P734 ?lastName.}
   SERVICE wikibase:label {
    bd:serviceParam wikibase:language "en" .
   }
 }
' > businesspersons.csv

#List public companies
curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?company ?companyLabel ?excahge ?excahgeLabel WHERE {
   ?company wdt:P414 ?excahge .
   ?company wdt:P31/wdt:P279* wd:Q43229 .
   SERVICE wikibase:label {
    bd:serviceParam wikibase:language "en" .
   }
 }
' > public_companies.csv


# List of subclass
curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?item ?itemLabel
WHERE
{
	?item wdt:P279/wdt:P279* wd:Q43229 .
	SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
}
' > orgs.csv

curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?item ?itemLabel
WHERE
{
	?item wdt:P279/wdt:P279* wd:Q215627 .
	SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
}
' > persons.csv

curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?item ?itemLabel
WHERE
{
	?item wdt:P279/wdt:P279* wd:Q12737077 .
	SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
}
' > occupations.csv

