from bs4 import BeautifulSoup

def getJELcodes(text):
    beg = text.find("{{JEL")
    tmp = text[beg:]
    param_beg = 1+tmp.find("|")
    end=tmp.find("}}")
    return tmp[param_beg:end]

soup = BeautifulSoup(open("Wikipedia-20170615063859.xml"), "xml")
pages = soup.find_all("page")

with open("jel.tsv", "w") as f:
    f.write('{0}\t{1}\t{2}\t{3}\t{4}\n'.format("title", "ns","page_id","JEL_code", "JEL_code_major"))
    for page in pages:
        aa=pages[0]
        ns = int(page.ns.text)
        #Keep category pages only.
        if ns!=14:
            continue
        #Drop "Category:" and replace white spaces
        title = page.title.text[9:].replace(" ","_")
        page_id = page.id.text 
        codes = getJELcodes(page.revision.text)
        for code in [x.strip() for x in codes.split(",")]:
            #Drop code with exostic formats
            if(len(code)>4):
                continue
            f.write('{0}\t{1}\t{2}\t{3}\t{4}\n'.format(title, ns,page_id,code,code[:1]))

