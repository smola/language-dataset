// WARNING: DO NOT EDIT THIS FILE. THIS FILE IS MANAGED BY SPRING ROO.
// You may push code into the target .java compilation unit if you wish to edit any member(s).

package cn.jhc.myexam.server.repository;

import cn.jhc.myexam.server.domain.Category;
import cn.jhc.myexam.server.repository.CategoryRepository;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

privileged aspect CategoryRepository_Roo_Jpa_Repository {
    
    declare parents: CategoryRepository extends JpaRepository<Category, Long>;
    
    declare parents: CategoryRepository extends JpaSpecificationExecutor<Category>;
    
    declare @type: CategoryRepository: @Repository;
    
}