class APPLICATION

create
	make

feature -- Initialization

	make
		do
			create cdb_server
			create cdb_document
			create cdb_database
--			test_server
--			test_database
			test_document
		end

	test_server
		do
			print ("%N COUCHDB SERVER API %N")
			print ("%N/:%N"+cdb_server.info)
			print ("%N_all_dbs:%N"+cdb_server.databases)
			print ("%N_config:%N"+cdb_server.config)
			print ("%N_uuids:%N" +cdb_server.uuids)
			print ("%N_stats:%N" +cdb_server.stats)
			print ("%N_active_tasks:"+ cdb_server.active_task)
		 	-- TODO cdb_server.replicate
		end

	test_database
		local
			l_response : STRING
			l_docs : STRING
		do
			print ("%N COUCHDB DATABASE API %N")
--			print("%NCreation%N"+cdb_database.db_create ("erest"))
--			print("%NInfo%N"+cdb_database.info ("erest"))
--			print("%NChange feed%N"+cdb_database.change_feed ("erest"))
--			--		print ("%NCompaction:%N" +cdb_database.compaction("erest"))
--			--
--			--		print("%NTemporary Views%N"+cdb_database.temp_view ("erest", Void))
--		 	l_docs := "{%"title%":%"eJSON: The Definitive Guide%",%"isbn%":%"123123-413243%",%"author%":{%"name%":%"Foo Bar%"}}"
--		 	print ("%NBulk docs:%N" +cdb_database.bulk_docs ("erest", l_docs))
--		 	print ("%NTemp view%N" + cdb_database.temp_view ("erest", "{}"))
--		 	print ("%NTemp view%N" + cdb_database.temp_view ("erest", l_docs))
			--print ("%NView cleanup%N" + cdb_database.view_cleanup ("erest"))
		end


	test_document
		local
			l_response : STRING
			l_docs : STRING
		do
--			print ("%NList all documents:%N"+ cdb_document.list ("erest"))
--			print ("%NCreate a document%N"+ cdb_document.doc_create ("erest", "{%"key%":%"value%"}%N"))
--			print ("%NGet Document%N"+ cdb_document.get ("erest", "7f549eb903ee29c3e66db6b039009c0f"))
--			print ("%NUpdate a document%N"+ cdb_document.update("erest/7f549eb903ee29c3e66db6b039009c0f","{%"key%":%"testupdate%"}%N"))
			print ("%NDelete a document%N"+ cdb_document.doc_delete("erest","7f549eb903ee29c3e66db6b039009c0f","1-59414e77c768bc202142ac82c2f129de"))
		end
feature -- Implementation

	cdb_server : CDB_SERVER
	cdb_document : CDB_DOCUMENT
	cdb_database : CDB_DATABASE

end
