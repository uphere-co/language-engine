import pandas as pd

# S&P 500 constituents
sp500 = pd.read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", header=0)[0]
sp500.to_csv("sp500companies.tsv", sep="\t", index=False)

# S&P 400 constituents
sp400 = pd.read_html("https://en.wikipedia.org/wiki/List_of_S%26P_400_companies", header=0)[0]
sp400.to_csv("sp400companies.tsv", sep="\t", index=False)
