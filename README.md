# Agda issue count stats

This contains a Python script that fetches all issues of the `github.com/agda/agda` repo.
A Haskell script then calculates the count of open and closed issues via some charts.
With some editing, this can be used on other repos as well.

## Python environment

For an offline export from an existing git-bug bridge mirror, skip to
[Fetching issues from git-bug](#fetching-issues-from-git-bug). This needs only
Python 3 and Git, without Python packages or a GitHub token.

Set up a python environment in the standard way, e.g., by following
https://stackoverflow.com/questions/43069780/how-to-create-virtual-env-with-python-3 .

        python3 -m venv .venv
        source .venv/bin/activate

Check that this is working by running `which python`.

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

However, fetching thousands of issues via individual GitHub API calls is still slow,
so the following approach using `git-bug bridge` is recommended.

## Fetching issues from git-bug

This method needs `git-bug` installed (e.g. `brew install git-bug` on macOS).

After updating your mirror with `git bug bridge pull` in `~/agda` (default), run:

        python3 import_git_bug.py

This writes `agda-YYYY-MM-DD-issues.json` in the current directory. To choose
another mirror or output file:

        python3 import_git_bug.py --git-bug-repo ~/agda -o agdaissues.json

This method just reads local Git objects and  makes no network requests.

Difference to API method (`import_issues.py`):
- Pull request are not tracked by `git-bug bridge`, so the JSON files will only contain the issues
  (and `is_pull_request` is always `false`).
- Also, Assignee information is not available, so the `assignee` is set to ``"None"`.

## Building the charts

Edit in `AgdaIssues.hs` some constants, e.g. concerning dates and input and output file names.
By default you only need to make a link `agdaissues.json` to the file created in the previous step.
The output file names are by default carrying today's date.

Run the script:

        cabal run

This leaves some `.png` files in the current directory containing
analysis charts of open and closed issues.

# Licence

The script `import_issues.py` has been modified from https://github.com/MorrisJobke/github-issue-counter which has been released under the MIT licence.

The script `import_git_bug.py` has been created by OpenAI's Codex (GPT-6.1-Sol).

The rest is (C) 2017-26 Andreas Abel released under BSD 3-clause.
