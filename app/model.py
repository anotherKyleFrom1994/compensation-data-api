from pydantic import BaseModel

# from datetime import datetime


class CompensationData(BaseModel):
    timestamp: str
    age: str
    industry: str
    job_title: str
    salary: int
    currency: str
    location: str
    work_exp: int
    job_title_context: str
    other_currency: str

    def __init__(__pydantic_self__, **data) -> None:
        super().__init__(**data)
        __pydantic_self__.timestamp = data["timestamp"]
        __pydantic_self__.age = data["age"]
        __pydantic_self__.industry = data["industry"]
        __pydantic_self__.job_title = data["job_title"]
        __pydantic_self__.salary = data["salary"]
        __pydantic_self__.currency = data["currency"]
        __pydantic_self__.location = data["location"]
        __pydantic_self__.work_exp = data["work_exp"]
        __pydantic_self__.job_title_context = data["job_title_context"]
        __pydantic_self__.other_currency = data["other_currency"]
