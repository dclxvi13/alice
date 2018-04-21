create table if not exists words (
    word text not null,
    form integer not null
)

create table if not exists links (
    startNode integer not null,
    endNode integer not null,
    weight integer
)

create table if not exists emotes (
    form integer not null
)