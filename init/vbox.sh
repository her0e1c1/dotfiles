v=vboxmanage

$v createvm --name mytest --register
$v modifyvm mytest --memory 1024 --acpi on --boot1 dvd --nic1 nat --pae on
$v createhd --filename /media/volume2/mytest.dvi --size 10000
$v storagectl mytest --name "IDE Controller" --add ide
$v storageattach mytest --storagectl "IDE Controller" --port 0 --device 0 --type hdd --medium /media/volume2/mytest.dvi

$v storageattach mytest --storagectl "IDE Controller" --port 1 --device 0 --type dvddrive --medium /media/volume2/ubuntu-12.04-desktop-i386.iso
