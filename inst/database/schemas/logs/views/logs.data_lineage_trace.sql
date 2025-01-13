CREATE VIEW logs.data_lineage_trace AS
WITH RECURSIVE lineage_tree AS (
    SELECT lineage_id, source_type, source_id, target_type, target_id, transformation_description, 1 AS level
    FROM data_lineage
    UNION ALL
    SELECT dl.lineage_id, dl.source_type, dl.source_id, dl.target_type, dl.target_id, dl.transformation_description, lt.level + 1
    FROM data_lineage dl
    JOIN lineage_tree lt ON dl.source_id = lt.target_id AND dl.source_type = lt.target_type
)
SELECT * FROM lineage_tree;
