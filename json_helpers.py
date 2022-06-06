#!/usr/bin/env python
# -*- coding: utf-8 -*-

# source: https://github.com/MorrisJobke/github-issue-counter/blob/e61c05a6dabd10c1af6ec4ce4263983ac99791fb/json_helpers.py

import json, datetime

class DateTimeEncoder(json.JSONEncoder):
  def default(self, obj):
    if isinstance(obj, datetime.datetime):
      return obj.isoformat()
    elif isinstance(obj, datetime.date):
      return obj.isoformat()
    elif isinstance(obj, datetime.timedelta):
      return (datetime.datetime.min + obj).time().isoformat()
    else:
      return super(DateTimeEncoder, self).default(obj)


def extract_datetime(s):
  if isinstance(s, datetime.datetime):
    return s
  return datetime.datetime.strptime(s[:-3] + s[-2:], '%Y-%m-%dT%H:%M:%S%z')
