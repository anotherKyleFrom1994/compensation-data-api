#!/usr/bin/env python
# coding: utf-8

import os
import re
from decimal import Decimal

import ujson as json
from fastapi import FastAPI, Request

from app.enums import *
from app.model import *

app = FastAPI()


@app.get("/")
def read_root():
    return {"Hello": " world"}


@app.get("/compensation_data")
def query_compensation_data(params: Request):
    raw_data = retrieve_compensation_data(
        f"{os.path.dirname(os.path.realpath(__file__))}/salary_survey-1.json"
    )

    params = params.query_params._dict
    compensation_data = [r for r in raw_data if match_criteria(r, params)]

    if "sort" in params:
        compensation_data.sort(key=lambda x: x[ColumnMapping.SALARY.value])

    compensation_data = [
        CompensationData(
            timestamp=process_timestamp(c[ColumnMapping.TIMESTAMP.value]),
            age=process_age(c[ColumnMapping.AGE.value]),
            industry=process_industry(c[ColumnMapping.INDUSTRY.value]),
            job_title=c[ColumnMapping.JOB_TITLE.value],
            salary=process_salary(c[ColumnMapping.SALARY.value]),
            currency=process_currency(c[ColumnMapping.CURRENCY.value]),
            location=process_location(c[ColumnMapping.LOCATION.value]),
            work_exp=process_work_exp(c[ColumnMapping.WORK_EXP.value]),
            job_title_context=process_job_title_context(
                c[ColumnMapping.JOB_TITLE_CONTEXT.value]
            ),
            other_currency=c[ColumnMapping.OTHER_CURRENCY.value],
        )
        for c in compensation_data
    ]
    return compensation_data


def match_criteria(raw_data: dict, criteria: dict):
    query_kv = {}
    operator_relation = {}
    for k, v in criteria.items():
        # ex. [salary, lte]
        query_str_arr = re.split(r"[\[\]]+", k)[:2]
        query_kv[query_str_arr[0]] = v

        if len(query_str_arr) > 1:
            operator_relation[query_str_arr[0]] = query_str_arr[1]
        else:
            operator_relation[query_str_arr[0]] = QueryOperator.EQ.value

    salary_match = True
    if "salary" in query_kv:
        salary = process_salary(raw_data[ColumnMapping.SALARY.value])
        target_sal = process_salary(query_kv["salary"])
        salary_match = QueryOperator.compare(
            salary, target_sal, operator_relation["salary"]
        )

    work_exp_match = True
    if "work_exp" in query_kv:
        work_exp = process_work_exp(raw_data[ColumnMapping.WORK_EXP.value])
        target_work_exp = int(query_kv["work_exp"])
        work_exp_match = QueryOperator.compare(
            work_exp,
            target_work_exp,
            operator_relation["work_exp"],
        )

    timestamp_match = True
    if "timestamp" in query_kv:
        ts = process_timestamp(raw_data[ColumnMapping.TIMESTAMP.value])
        target_ts = process_timestamp(query_kv["timestamp"])
        timestamp_match = QueryOperator.compare(
            ts,
            target_ts,
            operator_relation["timestamp"],
        )

    age_match = True
    if "age" in query_kv:
        pass

    industry_match = True
    if "industry" in query_kv:
        pass

    location_match = True
    if "location" in query_kv:
        pass

    return (
        salary_match
        and work_exp_match
        and timestamp_match
        and age_match
        and industry_match
        and location_match
    )


def process_timestamp(raw_ts):
    # datetime.strptime(raw_ts, "%m/%d/%Y %H:%M:%S")
    return raw_ts


def process_age(raw_age):
    return raw_age


def process_industry(raw_industry):
    return raw_industry


def process_salary(raw_salary):
    regex = re.compile(
        r"(?:(?:[1-9][0-9]{0,2})(?:,[0-9]{3})+|[1-9][0-9]*|0)(?:[\.,][0-9][0-9]?)?(?![0-9]+)"
    )
    raw_salary = regex.findall(raw_salary)
    salary = raw_salary[0] if len(raw_salary) != 0 else "0"
    salary = Decimal(re.sub(r"[^\d.]", "", salary))

    return salary


def process_currency(raw_currency):
    return raw_currency


def process_location(raw_location):
    return raw_location


def process_job_title_context(raw_job_title_context):
    return raw_job_title_context


def process_work_exp(raw_work_exp):
    return WorkExperience(raw_work_exp).transform()


def retrieve_compensation_data(file_loc: str):
    with open(file_loc) as f:
        raw_data = json.load(f)
        res = [
            r
            for r in raw_data
            if str.isnumeric(r[ColumnMapping.SALARY.value])
            and WorkExperience.has_value(r[ColumnMapping.WORK_EXP.value])
        ]
        return res
