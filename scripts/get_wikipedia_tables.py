import pandas as pd

# S&P 500 constituents
sp500 = pd.read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", header=0)[0]
sp500.Security = sp500.Security.str.replace(' ', '_')
sp500.to_csv("sp500companies.tsv", sep="\t", header=False, index=False)

# S&P 400 constituents
sp400 = pd.read_html("https://en.wikipedia.org/wiki/List_of_S%26P_400_companies", header=0)[0]
sp400.Company = sp400.Company.str.replace(' ', '_')
sp400.to_csv("sp400companies.tsv", sep="\t", header=False, index=False)
