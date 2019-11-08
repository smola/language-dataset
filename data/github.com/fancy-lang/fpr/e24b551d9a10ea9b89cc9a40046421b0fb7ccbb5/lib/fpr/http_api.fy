get: "/" do: {
  with_json_response: |p| {
    {
      packages: $ p packages for_json
    }
  }
}

get: "/list" do: {
  with_json_response: |p| {
    {
      packages: $ p package_names for_json
    }
  }
}

get: "/info/:repo_user/:package_name" do: |repo_user, package_name| {
  package_name = "#{repo_user}/#{package_name}"
  with_json_response: |p| {
    if: (p package: package_name) then: |package| {
      {
        package: $ package for_json
      }
    } else: {
      {
        error: "Package not found: '#{package_name}'"
      }
    }
  }
}

get: "/info/:repo_user/:package_name/:version" do: |repo_user, package_name, version| {
  package_name = "#{repo_user}/#{package_name}"
  with_json_response: |p| {
    if: (p package: package_name version: version) then: |package| {
      {
        package: $ package for_json
      }
    } else: {
      {
        error: "Package not found: '#{package_name}' version: '#{version}'"
      }
    }
  }
}


get: /\/search\/(.+)$/ do: |query| {
  with_json_response: |p| {
    {
      found_packages: $ p search_package: query for_json
    }
  }
}

get: "/add/:username/:reponame" do: |username, reponame| {
  package_name = "#{username}/#{reponame}"
  with_json_response: |p| {
    if: (p package: package_name) then: {
      {
        error: "Package not added."
        reason: "Package already exists: '#{username}/#{reponame}'"
      }
    } else: {
      package = p add_package: package_name
      if: package then: {
        {
          package_added: $ package for_json
          success: true
        }
      } else: {
        {
          error: "Could not add package: '#{username}/#{reponame}'"
          reason: "Does not exist or does not contain a valid .fancypack file."
        }
      }
    }
  }
}

