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
'  | tr -d '\r' > employees.csv

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
'  | tr -d '\r' > public_companies.csv


# List of subclass
curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?item ?itemLabel
WHERE
{
	?item wdt:P279* wd:Q43229 .
	SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
}
'  | tr -d '\r' > orgs.csv

curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?item ?itemLabel 
WHERE 
{
  ?item wdt:P279* wd:Q1496967.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
'  | tr -d '\r' > locations.csv

curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?item ?itemLabel
WHERE
{
	?item wdt:P279* wd:Q215627 .
	SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
}
'  | tr -d '\r' > persons.csv

curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?item ?itemLabel
WHERE
{
	?item wdt:P279* wd:Q12737077 .
	SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
}
'  | tr -d '\r' > occupations.csv

# List of Wikidata properteis
curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?property ?propertyLabel WHERE {
    ?property a wikibase:Property .
    SERVICE wikibase:label {
      bd:serviceParam wikibase:language "en" .
   }
 }

'  | tr -d '\r' > properties.csv


#subclass of brand (Q431289)
curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?item ?itemLabel WHERE {
  ?item wdt:P279* wd:Q431289. 
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
'  | tr -d '\r' > brands.csv

#subclass of human made rules
curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?item ?itemLabel 
WHERE 
{
  ?item wdt:P279* wd:Q1151067.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
'  | tr -d '\r' > human_rules.csv

#subclass of buildings
curl -H "Accept: text/csv" -G https://query.wikidata.org/sparql --data-urlencode query='
SELECT ?item ?itemLabel 
WHERE 
{
  ?item wdt:P279* wd:Q41176.
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
'  | tr -d '\r' > buildings.csv
