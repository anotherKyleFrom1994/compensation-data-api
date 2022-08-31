from enum import Enum


class BaseEnum(Enum):
    @classmethod
    def has_value(cls, value):
        return value in cls._value2member_map_


class WorkExperience(BaseEnum):
    EXP1_LESS = "1 year or less"
    EXP2_4 = "2 - 4 years"
    EXP5_7 = "5-7 years"
    EXP8_10 = "8 - 10 years"
    EXP11_20 = "11 - 20 years"
    EXP21_30 = "21 - 30 years"
    EXP31_40 = "31 - 40 years"
    EXP41_MORE = "41 years or more"
    EXP65_OVER = "65 or over"

    def transform(cls):
        if cls.value == WorkExperience.EXP1_LESS.value:
            return 0
        elif cls.value == WorkExperience.EXP2_4.value:
            return 2
        elif cls.value == WorkExperience.EXP5_7.value:
            return 5
        elif cls.value == WorkExperience.EXP8_10.value:
            return 8
        elif cls.value == WorkExperience.EXP11_20.value:
            return 11
        elif cls.value == WorkExperience.EXP21_30.value:
            return 21
        elif cls.value == WorkExperience.EXP31_40.value:
            return 31
        elif cls.value == WorkExperience.EXP41_MORE.value:
            return 41
        elif cls.value == WorkExperience.EXP65_OVER.value:
            return 65


class ColumnMapping(BaseEnum):
    TIMESTAMP = "Timestamp"
    AGE = "How old are you?"
    INDUSTRY = "What industry do you work in?"
    JOB_TITLE = "Job title"
    SALARY = "What is your annual salary?"
    CURRENCY = "Please indicate the currency"
    LOCATION = "Where are you located? (City/state/country)"
    WORK_EXP = (
        "How many years of post-college professional work experience do you have?"
    )
    JOB_TITLE_CONTEXT = (
        "If your job title needs additional context, please clarify here:"
    )
    OTHER_CURRENCY = 'If "Other," please indicate the currency here:'


class QueryOperator(BaseEnum):
    GT = "gt"
    LT = "lt"
    GTE = "gte"
    LTE = "lte"
    EQ = "eq"

    @classmethod
    def compare(cls, o1, o2, enum_value):
        if enum_value == QueryOperator.GTE.value:
            return o1 >= o2
        elif enum_value == QueryOperator.GT.value:
            return o1 > o2
        elif enum_value == QueryOperator.LTE.value:
            return o1 <= o2
        elif enum_value == QueryOperator.LT.value:
            return o1 < o2
        else:
            return o1 == o2
