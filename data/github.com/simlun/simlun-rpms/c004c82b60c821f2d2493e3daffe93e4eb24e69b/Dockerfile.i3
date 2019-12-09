FROM centos:8
RUN dnf -y install epel-release
RUN dnf -y install 'dnf-command(config-manager)'
RUN dnf config-manager --set-enabled PowerTools
RUN dnf -y install gcc rpm-build rpm-devel rpmlint make bash diffutils patch rpmdevtools git

RUN rpmdev-setuptree

WORKDIR /root

# List of fedora 28 mirrors at:
# https://mirrors.fedoraproject.org/mirrorlist?repo=fedora-28&arch=x86_64
ARG f28=https://ftp-stud.hs-esslingen.de/pub/Mirrors/archive.fedoraproject.org/fedora/linux/releases/28/Everything/source/tree/Packages/

# xcb-util-xrm
ARG xcbutilxrm=xcb-util-xrm-1.2-5.fc28.src.rpm
RUN curl -O $f28/x/$xcbutilxrm
RUN dnf -y install $(rpm -q --requires -p $xcbutilxrm | cut -f 1 -d ' ' | grep -v rpmlib | xargs)
RUN rpmbuild --rebuild $xcbutilxrm
RUN dnf -y install /root/rpmbuild/RPMS/x86_64/xcb-util-xrm*.rpm

# i3
ARG i3=i3-4.15-1.fc28.src.rpm
RUN curl -O $f28/i/$i3
RUN dnf -y install $(rpm -q --requires -p $i3 | cut -f 1 -d ' ' | grep -v rpmlib | xargs)
RUN rpmbuild --rebuild $i3

# dmenu
ARG dmenu=dmenu-4.8-1.fc28.src.rpm
RUN curl -O $f28/d/$dmenu
RUN dnf -y install coreutils-single
RUN dnf -y install $(rpm -q --requires -p $dmenu | cut -f 1 -d ' ' | grep -v rpmlib | grep -v coreutils | xargs)
RUN rpmbuild --rebuild $dmenu
RUN dnf -y install /root/rpmbuild/RPMS/x86_64/dmenu*.rpm

# dzen2
ARG dzen2=dzen2-0.8.5-21.20100104svn.fc28.src.rpm
RUN curl -O $f28/d/$dzen2
RUN dnf -y install $(rpm -q --requires -p $dzen2 | cut -f 1 -d ' ' | grep -v rpmlib | grep -v coreutils | xargs)
RUN rpmbuild --rebuild $dzen2
RUN dnf -y install /root/rpmbuild/RPMS/x86_64/dzen2*.rpm

# wireless-tools-devel
ARG wirelesstools=wireless-tools-29-18.1.fc28.src.rpm
RUN curl -O $f28/w/$wirelesstools
RUN rpmbuild --rebuild $wirelesstools
RUN dnf -y install /root/rpmbuild/RPMS/x86_64/wireless-tools*.rpm

# i3status
ARG i3status=i3status-2.11-7.fc28.src.rpm
RUN curl -O $f28/i/$i3status
RUN dnf -y install $(rpm -q --requires -p $i3status | cut -f 1 -d ' ' | grep -v rpmlib | grep -v coreutils | xargs)
RUN rpmbuild --rebuild $i3status
RUN dnf -y install /root/rpmbuild/RPMS/x86_64/i3status*.rpm

# perl(AnyEvent::I3)
ARG perlanyeventi3=perl-AnyEvent-I3-0.17-4.fc28.src.rpm
RUN curl -O $f28/p/$perlanyeventi3
RUN dnf -y install $(rpm -q --requires -p $perlanyeventi3 | cut -f 1 -d ' ' | grep -v rpmlib | grep -v coreutils | xargs)
RUN rpmbuild --rebuild $perlanyeventi3
RUN dnf -y install /root/rpmbuild/RPMS/noarch/perl-AnyEvent-I3*.rpm

# Finally do i3 install for dependency checking
RUN dnf -y install /root/rpmbuild/RPMS/x86_64/i3-*.rpm
