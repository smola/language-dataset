SELECT 
--  bib_record.record_id as bid, -- We need this id to connect bib_record to record_metadata
  '.b' || record_metadata.record_num || 'a' as bnum,  -- I added in the '.b' at the beginning and 'a' (in place of the check digit) at the end so it looks more like a normal bib rec number. 
  bib_record_location.location_code as bloc
--  bib_record_location.display_order, -- we need this value because (I think) we want the first bib location listed. In records I checked, "multi" was the first displayed bib location
--  order_record.record_id as oid, - We need this id to connect bib_record through order_record to order_record_cmf
  --order_record_cmf.location_code || '(' || order_record_cmf.copies || ')' as LocCopy -- concatenating fields to format the column's data
  FROM
  sierra_view.bib_record, -- the bib record
  sierra_view.record_metadata, -- has record_num that we need for the lookup
  sierra_view.bib_record_location --bib location field
WHERE
  bib_record.record_id = record_metadata.id
  and record_metadata.record_type_code = 'b'
  and record_metadata.record_num = '4451800'
  