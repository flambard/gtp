-record(protocol_version, {}).

-record(quit, {}).

-type command() :: #protocol_version{} | #quit{}.
