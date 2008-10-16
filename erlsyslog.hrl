%% Specify levels.

-define(LOG_EMERGENCY, 0). % system is unusable
-define(LOG_ALERT,     1). % action must be taken immediately
-define(LOG_CRITICAL,  2). % critical conditions
-define(LOG_ERROR,     3). % error conditions
-define(LOG_WARNING,   4). % warning conditions
-define(LOG_NOTICE,    5). % normal but significant condition
-define(LOG_INFO,      6). % informational
-define(LOG_DEBUG,     7). % debug-level messages

% facility codes
-define(FAC_KERN,        (0 bsl 3)). % kernel messages
-define(FAC_USER,        (1 bsl 3)). % random user-level messages
-define(FAC_MAIL,        (2 bsl 3)). % mail system
-define(FAC_DAEMON,      (3 bsl 3)). % system daemons
-define(FAC_AUTH,        (4 bsl 3)). % security/authorization messages
-define(FAC_SYSLOG,      (5 bsl 3)). % messages generated internally by syslogd
-define(FAC_LPR,         (6 bsl 3)). % line printer subsystem
-define(FAC_NEWS,        (7 bsl 3)). % network news subsystem
-define(FAC_UUCP,        (8 bsl 3)). % UUCP subsystem
-define(FAC_CRON,        (9 bsl 3)). % clock daemon
-define(FAC_AUTHPRIV,   (10 bsl 3)). % security/authorization messages (private)
-define(FAC_FTP,        (11 bsl 3)). % ftp daemon

% other codes through 15 reserved for system use
-define(FAC_LOCAL0,     (16 bsl 3)). % reserved for local use
-define(FAC_LOCAL1,     (17 bsl 3)). % reserved for local use
-define(FAC_LOCAL2,     (18 bsl 3)). % reserved for local use
-define(FAC_LOCAL3,     (19 bsl 3)). % reserved for local use
-define(FAC_LOCAL4,     (20 bsl 3)). % reserved for local use
-define(FAC_LOCAL5,     (21 bsl 3)). % reserved for local use
-define(FAC_LOCAL6,     (22 bsl 3)). % reserved for local use
-define(FAC_LOCAL7,     (23 bsl 3)). % reserved for local use

