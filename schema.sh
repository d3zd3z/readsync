#! bin/sh

# Pass these in.
server="$1"
user="$2"
pass="$3"

folders='CAF CAF.linaro-dev CAF.linux-arm-kernel CAF.linux-arm-msm CAF.linux-kernel CAF.devicetree CAF.linux-next'
sfolder=''
for i in $folders; do
	sfolder="$sfolder
insert into folders (name, validity) values('$i', 0);"
done

# Create the database.
mv -f rs-state.db rs-state.bak
sqlite3 rs-state.db <<Z
create table server (user text, pass text, host text);
insert into server values('$user', '$pass', '$server');
create table folders (
   key integer primary key autoincrement,
   name text,
   validity int64
);
$sfolder

create table idmap (
   folderKey integer references folders(key),
   validity int64,
   uid int64,
   messageid blob,
   seen boolean);
create index idmap_mid on idmap (messageid);
create index idmap_id on idmap (validity, uid);
Z
