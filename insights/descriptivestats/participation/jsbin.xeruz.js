var n1=0;
var n2=0;
var decide=-1;
var fre=0;
function run() {
n1=document.getElementById("Ultra").value

decide=fre+" "+(n1).toString()+"-"+(n1).toString()
console.log(decide);
$('#chart1').empty();
if(n1>0 && n2>0){
visualizeBarChart(decide);
}

}

function run2() {

n2=document.getElementById("Ultra2").value

if ( n2==1) {
    fre='leadership';
} else if (n2==2) {
    fre='participation';
} else if (n2==3) {
    fre='attendance';
} 
decide=fre+" "+(n1).toString()+"-"+(n1).toString()
console.log(decide);
$('#chart1').empty();
if(n1>0 && n2>0){
visualizeBarChart(decide);
}
}



function visualizeBarChart(filenumber) {

var svg = d3.select('svg').append('svg').attr({
  width: 600,
  height: 600,
  border: '1px solid #ccc'
});

svg.append('svg:image')
.attr({
  'xlink:href': "descriptivestats/participation/images/"+filenumber+".png",  // can also add svg file here
  x: 0,
  y: 0,
  width: 600,
  height: 600
});
console.log("descriptivestats/participation/images/"+filenumber+".png")


    }





