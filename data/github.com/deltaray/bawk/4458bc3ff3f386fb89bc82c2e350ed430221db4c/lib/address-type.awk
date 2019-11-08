# Copyright 2017 Mark Krenz <mkrenz@iu.edu>
#    This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Function for determining IPv4 or IPv6
# Based on the format of the string. This needs to be improved.
function addrtype(_address_) {
    if (index(_address_, ".")) {
        _addrtype_ = "v4"
    } else if (index(_address_, ":")) {
        _addrtype_ = "v6"
    } else {
        _addrtype_ = ""
    }
    return _addrtype_
}


