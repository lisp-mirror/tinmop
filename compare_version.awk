# tinmop: an humble mastodon client
# Copyright (C) 2020  cage

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.
# If not, see [[http://www.gnu.org/licenses/][http://www.gnu.org/licenses/]].

BEGIN {
    TRUE        = 1;
    FALSE       = 0;
    VERSION_SEP = "\\.";
}

function split_version_number (version, parsed) {
    split(version, parsed, VERSION_SEP);
    for (i in parsed) {
        parsed[i] = strtonum(parsed[i]);
    }
}

function version_less_than_p (version_a, version_b,     idx) {
    for (idx=1; idx <= length(version_a); idx++){
        if(version_a[idx] < version_b[idx]){
            return TRUE;
        } else if(version_a[idx] > version_b[idx]){
            return FALSE;
        }
    }
    return FALSE;
}


// {
    versions[""]="";
    version_a[""]="";
    version_b[""]="";
    split($0, versions, "[[:space:]]+");
    split_version_number(versions[1], version_a);
    split_version_number(versions[2], version_b);
    print (version_less_than_p(version_a, version_b));
}
