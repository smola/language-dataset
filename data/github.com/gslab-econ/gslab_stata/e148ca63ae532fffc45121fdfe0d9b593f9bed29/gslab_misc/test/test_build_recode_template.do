quietly {
	cd test
    version 11
    set more off
    adopath + ../ado
	preliminaries
	
	program main
        cap rmdir temp
		cap mkdir temp
		quietly setup_main_dataset, obs(100) output(temp/main.dta) keymin(0)
		testgood test_basic
		testgood test_keyed
		testgood test_descriptors
		
		quietly setup_import_dataset
		testgood test_import
		
		quietly extend_main_dataset, obs(10) appendto(temp/main.dta)
		testgood test_import_extended

		testbad test_missing_attribute
		
		clean_up
	end
	
	program setup_main_dataset
		syntax, obs(int) output(str) keymin(int)
		preserve
		set obs `obs'
		gen key = mod(_n,2) + `keymin'
		gen attribute1 = round(runiform(), 0.0000001)
		gen attribute2 = round(runiform(), 0.0000001)
		gen attribute3 = floor(runiform()*10)
		gen attribute1_description = "attribute1"
		gen attribute2_description = "attribute2"
		gen attribute3_description = "attribute3"
		gen descriptor1 = "attribute1"
		gen descriptor2 = "attribute2"
        gen quantity = floor(runiform()*10)
		save `output', replace
		restore
	end

	program test_basic
		preserve
		build_recode_template using temp/main.dta, output(temp/recode1.csv) variables(attribute1 attribute2 attribute3)
		insheet using temp/recode1.csv, comma clear
		describe
		assert r(k) == 5
		capture confirm variable attribute code description recode notes
		assert !_rc
		restore
	end
	
	program test_keyed
		preserve
		build_recode_template using temp/main.dta, output(temp/recode2.csv) variables(attribute1 attribute2 attribute3) ///
			key(key)
		insheet using temp/recode2.csv, comma clear
		describe
		assert r(k) == 6
		capture confirm variable key attribute code description recode notes
		assert !_rc		
		restore
	end

	program test_descriptors
		preserve
		build_recode_template using temp/main.dta, output(temp/recode3.csv) variables(attribute1 attribute2 attribute3) ///
			descriptors(descriptor1 descriptor2)
		insheet using temp/recode3.csv, comma clear
		describe
		assert r(k) == 5
		capture confirm variable attribute code description recode notes
		assert !_rc
		forvalues i = 1/2{
			assert description == "attribute"+"`i'" if attribute == "attribute"+"`i'"
		}
		restore
	end
	
	program setup_import_dataset
		preserve
		insheet using temp/recode2.csv
		replace recode = 1 if _n<=10
        ren recode newcode
        drop notes
		gen notes = "newcode" if _n<=10
        outsheet using temp/recode4.csv, comma replace
		restore
	end

	program test_import
		preserve
		build_recode_template using temp/main.dta, output(temp/recode5.csv) variables(attribute1 attribute2 attribute3) /// 
			key(key) import(temp/recode4.csv) recode_var_name(newcode)
		insheet using temp/recode5.csv, comma clear
		describe
		assert r(k) == 6
		capture confirm variable key attribute code description newcode notes
		assert !_rc	
		summarize newcode
		assert r(sum) == 10	
		assert notes == "newcode" if newcode == 1
		assert notes == "" if newcode == .
		restore
	end
	
	program extend_main_dataset
		syntax, obs(int) appendto(str)
		preserve
		cap describe using `appendto'
		tempfile extend
		setup_main_dataset, obs(10) output(`extend') keymin(`r(N)')
		use `appendto', replace
		append using `extend'
		save `appendto', replace	
		restore
	end
	
	program test_import_extended
		preserve
		build_recode_template using temp/main.dta, output(temp/recode6.csv) variables(attribute1 attribute2 attribute3) ///
			key(key) import(temp/recode4.csv) recode_var_name(newcode)
		insheet using temp/recode6.csv, comma clear
		describe
		assert r(k) == 6
		assert r(N) == 249
		capture confirm variable key attribute code description newcode notes
		assert !_rc	
		summarize newcode
		assert r(sum) == 10	
		assert notes == "newcode" if newcode == 1
		assert notes == "" if newcode == .
		restore
	end

	program test_quantity_and_count
		preserve
		build_recode_template using temp/main.dta, output(temp/recode9.csv) variables(attribute3) ///
            key(key) count quantity_vars(quantity)
        insheet using temp/recode9.csv, clear
        describe
		
		restore
	end

	program test_missing_attribute
		preserve
		use temp/main.dta
		replace attribute1 = .
		save temp/main.dta, replace
		build_recode_template using temp/main.dta, output(temp/recode8.csv) variables(attribute1 attribute2 attribute3)
		restore
	end
	
    program clean_up
        erase temp/main.dta
		forvalues i = 1/6{
			erase temp/recode`i'.csv
		}
        rmdir temp
    end	
	
}

* EXECUTE
main
