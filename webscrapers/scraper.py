import requests
from bs4 import BeautifulSoup
from datetime import datetime
from datetime import timedelta
from datetime import date
import csv
import pandas as pd
import time
import json
import fnmatch
import os
import tabula
from tabula.io import read_pdf
from datetime import date


def requests_get_item(url, item):
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36"}
    if item == "html":
        page = requests.get(url, headers=headers)
        soup = BeautifulSoup(page.text, 'html.parser')
        return(soup)

    elif item == "pdf":
        page = requests.get(url, headers=headers, stream=True)
        return(page)
    else:
        print("Valid Item Not Selected")


def download_pdfs(ls_pdf_urls, download_path, file_name):
    counter = 0
    for pdf_url in ls_pdf_urls:
        counter = counter + 1
        g = requests_get_item(pdf_url, "pdf")
        with open(f'{download_path}{file_name}_{counter}.pdf', 'wb') as sav:
            for chunk in g.iter_content(chunk_size=1000000):
                sav.write(chunk)
        print(f"download number: {counter}")


def convert_pdf_to_csv(pdf_directory, csv_directory):
    directory = fr'{pdf_directory}'
    directory_output = fr'{csv_directory}'
    count = 0
    for file in os.listdir(directory):
        if file.endswith(".pdf"):
            count = count + 1
            print(f'{directory}{file}: Conversion {count}')
            tabula.convert_into(
                f'{directory}{file}', f'{directory_output}{count}.csv', output_format="csv", pages='all')


def scrape_umd_arrest(date):

    # Initialize arrays to save data
    arrest_number = []
    arrested_date_time_charge = []
    umpd_case_number = []
    age = []
    sex = []
    race = []
    description = []

    # Define years to scrape
    years = list(range(2010, 2022))

    for year in years:
        url = f"https://www.umpd.umd.edu/stats/arrest_report.cfm?year={year}"
        page = requests.get(url)
        soup = requests_get_item(url, "html")
        ls_rows = soup.find_all("tr")
        ls_rows_noHeaders = ls_rows[1:]
        num_rows = len(ls_rows_noHeaders)//2
        grouped = list(zip(*[iter(ls_rows_noHeaders)]*2))
        for i in grouped:
            subRow1 = i[0].find_all("td")
            subRow2 = i[1].find_all("td")

            arrest_number.append(subRow1[0].text.strip())
            arrested_date_time_charge.append(subRow1[1].text.strip())
            umpd_case_number.append(subRow1[2].text.strip())
            age.append(subRow1[3].text.strip())
            race.append(subRow1[4].text.strip())
            sex.append(subRow1[5].text.strip())
            description.append(subRow2[0].text.strip())

    data = {"arrest_number": arrest_number, "arrested_date_time_charge": arrested_date_time_charge,
            "umpd_case_number": umpd_case_number, "race": race, "age": age, "sex": sex, "description": description}
    today = date.today()
    df = pd.DataFrame(data)
    df.to_csv(f"data/raw/arrests_data_{today}.csv")


def scrape_umd_incident(date):
    # get years and months
    years_initial = list(range(2014, 2022))
    years = [item for item in years_initial for i in range(12)]
    num_months = len(years)
    months = []
    for i in range(1, num_months+1):
        months.extend(list(range(1, 13)))
    zipped = zip(years, months)
    year_month_pair = list(zipped)

    # Download

    umpd_case_number = []
    occured_date_time_location = []
    report_date_time = []
    _type = []
    disposition = []
    location = []

    #years = list(range(2010, 2021))
    print(year_month_pair)
    for pair in year_month_pair:
        year = pair[0]
        month = pair[1]
        print(year, month)
        print("=-----=")
        url = f"https://www.umpd.umd.edu/stats/incident_logs.cfm?year={year}&month={month}"
        page = requests.get(url)
        soup = BeautifulSoup(page.text, 'html.parser')
        ls_rows = soup.find_all("tr")
        ls_rows_noHeaders = ls_rows[1:]
        num_rows = len(ls_rows_noHeaders)//2
        grouped = list(zip(*[iter(ls_rows_noHeaders)]*2))
        for i in grouped:
            subRow1 = i[0].find_all("td")
            subRow2 = i[1].find_all("td")

            umpd_case_number.append(subRow1[0].text.strip())
            occured_date_time_location.append(subRow1[1].text.strip())
            report_date_time.append(subRow1[2].text.strip())
            _type .append(subRow1[3].text.strip())
            disposition.append(subRow1[4].text.strip())
            location.append(subRow2[0].text.strip())

    data = {"umpd_case_number": umpd_case_number, "occured_date_time_location": occured_date_time_location,
            "report_date_time": report_date_time, "_type": _type, "disposition": disposition, "location": location}
    today = date.today()
    df = pd.DataFrame(data)
    df.to_csv(f"data/raw/incidents_data_{today}.csv")


today = date.today()
scrape_umd_arrest(today)
scrape_umd_incident(today)
