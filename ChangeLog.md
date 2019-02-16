# Changelog for lunch-picker

## Pre-release changes

* 2019-02-16: Set `enterprise_id` as an optional part of the
  SlackFormData type. This was not being included in some events from
  Slack, causing a default empty user list to take over in
  `parseUserList` and removing all vetoes from consideration.
