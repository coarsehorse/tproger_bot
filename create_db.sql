/*
  One article has many tags
  One tag is about many articles
*/
CREATE TABLE tag (
  tag_id	serial PRIMARY KEY
, tag_text	text NOT NULL UNIQUE
);

CREATE TABLE article (
  article_id	serial	PRIMARY KEY
, article_url	text	NOT NULL UNIQUE
, article_time	timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE tag_article (
  tag_id    int REFERENCES tag (tag_id) ON UPDATE CASCADE ON DELETE CASCADE
, article_id int REFERENCES article (article_id) ON UPDATE CASCADE ON DELETE CASCADE
, CONSTRAINT bill_product_pkey PRIMARY KEY (tag_id, article_id)
);