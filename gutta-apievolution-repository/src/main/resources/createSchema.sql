create sequence seq_provider_apis increment by 50;

create table ProviderApiDefinitions (
    id integer primary key,
    historyName varchar(255),
    revisionNumber integer,
    commitTime timestamp,
    definitionText clob
);

create sequence seq_consumer_apis increment by 50;

create table ConsumerApiDefinitions (
    id integer primary key,
    commitTime timestamp,
    consumerName varchar(255),
    referencedRevision_id integer,
    definitionText clob
);
