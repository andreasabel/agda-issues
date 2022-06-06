# Agda issue count stats

This contains a Python script that fetches all issues of the `github.com/agda/agda` repo.
A Haskell script then calculates the count of open and closed issues via some charts.
With some editing, this can be used on other repos as well.

## Requirements

Install the requirements

        pip install github3.py

or

        pip install -r requirements.txt

Get an API key for GitHub and set it as `GITHUB_TOKEN` in your environment.

## Fetching issues

Edit in `import_issues.py` the variable `FILENAME_ISSUES`
(and possibly `ORG` and `REPO`).

Run fetcher part:

        ./import_issues.py

This will fetch all issues and save the needed information to a JSON
file (as given by `FILENAME_ISSUES`) to work with in the next step,
which saves GitHub API requests and speeds the overall process up.

## Building the charts

Edit in `AgdaIssues.hs` some constants, e.g. concerning dates and input and output file names.

Run the script:

        cabal run

This leaves some `.png` files in the current directory containing
analysis charts of open and closed issues.

# Licence

The script `import_issues.py` has been modified from https://github.com/MorrisJobke/github-issue-counter which has been released under the MIT licence.

The rest is (C) 2017-22 Andreas Abel released under BSD 3-clause.
