#! bin/sh

DB=rs-state.db

if [ -f "$DB" ]; then
	sqlite3 "$DB" '.dump server' > /tmp/server$$
	rm "$DB"
	sqlite3 "$DB" < /tmp/server$$
	rm /tmp/server$$
else
	# Pass these in.
	server="$1"
	user="$2"
	pass="$3"

	sqlite3 "$DB" <<Z
create table server (user text, pass text, host text);
insert into server values('$user', '$pass', '$server');
Z
fi

folders='CAF CAF.linaro-dev CAF.linux-arm-kernel CAF.linux-arm-msm CAF.linux-kernel CAF.devicetree CAF.linux-next CAF.rtc-linux CAF.git'
sfolder=''
for i in $folders; do
	sfolder="$sfolder
insert into folders (name, validity) values('$i', 0);"
done

# Create the database.
sqlite3 rs-state.db <<Z
create table folders (
   key integer primary key autoincrement,
   name text not null,
   validity int64 not null
);
$sfolder

create table idmap (
   folderKey integer references folders(key) not null,
   uid int64 not null,
   messageid blob not null,
   seen boolean not null);
create index idmap_mid on idmap (messageid);
create index idmap_id on idmap (folderKey, uid);
Z
