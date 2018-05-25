import requests
from bs4 import BeautifulSoup
import time
import os

def find_pdfs(html_file):
    with open(html_file) as fp:
        soup = BeautifulSoup(fp)
    links = soup.find_all("a")
    pdf_links = []
    for l in links:
        href = l.get("href")
        if href[-4:] == ".pdf" and href.find("generalelections2018") != -1:
            pdf_links.append(href)
    return pdf_links

def download_pdf(link, folder):
    if not os.path.exists(folder):
        os.makedirs(folder)
    res = requests.get(link)
    fname = folder + os.path.basename(link)
    if res.status_code == 200:
        with open(fname, 'wb') as f:
            f.write(res.content)
        return None
    else:
        return link
        

def main(source, outf):
    links = find_pdfs(source)
    failed_dlds = []
    print(".")
    print(source)
    print(len(links))
    for i, link in enumerate(links):
        if i % 10 == 0:
            print(i)
        out = download_pdf(link, outf)
        if out is not None:
            failed_dlds.append(out)

    print(failed_dlds)
    if len(failed_dlds) > 1:
        with open(source + "_failed", "w") as f:
            f.writelines(failed_dlds)
    elif len(failed_dlds):
        with open(source + "_failed", "w") as f:
            f.write(failed_dlds)
    

main("kp.html", "../raw/kp/pdf/")
main("fata.html", "../raw/fata/pdf/")
main("punjab.htm", "../raw/punjab/pdf/")
main("balochistan.html", "../raw/balochistan/pdf/")
main("sindh.htm", "../raw/sindh/pdf/")

