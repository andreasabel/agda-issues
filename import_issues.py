#!/usr/bin/env python3

# adapted from
# https://github.com/MorrisJobke/github-issue-counter/blob/e61c05a6dabd10c1af6ec4ce4263983ac99791fb/import_issues.py

import github3, json, os, os.path

from json_helpers import DateTimeEncoder

GITHUB_TOKEN = os.environ['GITHUB_TOKEN']
ORG = 'agda'
REPO = 'agda'
FILENAME_ISSUES = ORG + '-2022-06-06-' + 'issues.json'

data = {}

if os.path.isfile(FILENAME_ISSUES):
  print("- reading ", FILENAME_ISSUES)
  f = open(FILENAME_ISSUES)
  data = json.load(f)
  f.close()

print ("- logging in to github")
gh = github3.login(token=GITHUB_TOKEN)

if REPO not in data.keys():
  data[REPO] = {}

print ("- getting issues")
# https://github.com/MorrisJobke/github-issue-counter/issues/2#issuecomment-462488250
issues = gh.issues_on(ORG, REPO, state='all')

print ("- iterating through issues")
for i, issue in enumerate(issues):
  print (i, end=' ', flush=True)
  data[REPO][issue.number] = {
      'created_at'      : issue.created_at
    , 'closed_at'       : issue.closed_at
    , 'is_pull_request' : (issue.pull_request() is not None)
    , 'assignee'        : str(issue.assignee)
    # Trying to obtain the labels, I hit "403 API rate limit exceeded"
    # , 'labels'          : [l.name for l in issue.labels()]
    }
print ("- downloaded ", i, " issues")

print ("- writing ", FILENAME_ISSUES)
f = open(FILENAME_ISSUES, 'w')
json.dump(data, f, cls=DateTimeEncoder, indent=2)
f.close()
