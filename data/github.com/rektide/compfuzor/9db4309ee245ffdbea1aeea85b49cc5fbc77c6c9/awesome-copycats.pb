---
- hosts: all
  vars:
    TYPE: awesome-copycat
    REPO: https://github.com/copycat-killer/awesome-copycats.git
    ENV:
      CONFDIR: "{{XDG_CONFIG_DIR}}/awesome"
    BINS:
    - name: install.sh
      exec: |
        CONFDIR=${CONFDIR/#\~/$HOME}
        [ -d "$CONFDIR" ] || mkdir ${CONFDIR/#\~/$HOME}
        cd $CONFDIR
        while read file
        do
          localname=${file#$DIR/}
          [ -e "$localname" ] || ([ -d "$file" ] && mkdir "$localname") || ln -s "$file" "$localname"
        done < <(find $DIR)
  tasks:
  - include: tasks/compfuzor.includes type=src
