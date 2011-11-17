#! bin/sh

DB=${HOME}/.readsync.db

readsecret()
{
	echo -n "$1: " >&2
	oldmode=`stty -g`
	stty -echo
	read pass
	stty "$oldmode"
	echo '' >&2
	echo $pass
}

readnonsecret()
{
	echo -n "$1: " >&2
	read text
	echo $text
}

if [ -f "$DB" ]; then
	sqlite3 "$DB" '.dump server' > /tmp/server$$
	rm "$DB"
	sqlite3 "$DB" < /tmp/server$$
	rm /tmp/server$$
else
	# Prompt the user for this information.
	server=`readnonsecret server host`
	user=`readnonsecret user`
	pass=`readsecret password`

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
