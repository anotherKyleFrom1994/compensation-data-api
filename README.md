# compensation-data-api

This repo contains a backend API constructed with FastAPI in purpose of quick start a API endpoint. Currently only part of the compensation data is serialized, and part of the data has not been strictly formatted into database friendly format.

Check the next step in [ROADMAP.md](./ROADMAP.md)

# Project setup (require Python3+)

Run the server locally with uvicorn.

```bash
chmod +x ./setup.sh
./setup.sh
```

You should see the api doc via this URL: \
http://localhost:8000/docs

# Supported Query

The api supports operators for every queries. There are multiple operators:

- gte
- gt
- lte
- lt

Send a GET request with the following format to query by criteria:

http://localhost:8000/compensation_data?salary[le]=1000000&work_exp[gte]=2
