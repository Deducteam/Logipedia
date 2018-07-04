db.system.js.save({
  _id : "recursiveDep" ,
  value : function (mod, name,tabRetour){ 
    var res = db.dependances.find({md : mod, nameID : name});
    res.forEach(function(resultat){
      var tmp = [resultat.mdDep.toString(), resultat.idDep.toString()];
      if(resultat.mdDep.toString()!== '' && resultat.mdDep.toString()!== null  && !itemInArray(tabRetour,tmp)){
       
        tabRetour.push(tmp);
        
print("tabRetour : "); 
print(tabRetour); 
        return recursiveDep(resultat.mdDep.toString(),resultat.idDep.toString(),tabRetour);
      }
    });
  }
});


db.system.js.save({
  _id : "itemInArray" ,
  value : function (array,item){ 
        for (var i = 0; i < array.length; i++) {
        // This if statement depends on the format of your array
        if (array[i][0] == item[0] && array[i][1] == item[1]) {
            return true;   // Found it
        }
    }
    return false;   // Not found
  }
});


db.system.js.save({
  _id : "recursiveDep" ,
  value : function (mod, name,tabRetour){ 
    var res = db.dependances.find({md : mod, nameID : name});
    res.forEach(function(resultat){
      if(!resultat['mdDep'] && !tabRetour.includes(tmp) && resultat['mdDep']!=resultat['md']){
        return db.dependances.aggregate( [
   {
      $graphLookup: {
         from: "dependances",
         startWith: "$md",
         connectFromField: "md",
         connectToField: "mdDep",
         as: "reportingHierarchy"
      }
   }
] );
      }
    });
  }
});
db.dependances.aggregate( [
{ $match: { $ne: [ "$md", "$mdDep" ] } },
   {
      $graphLookup: {
         from: "dependances",
         startWith: {"$md","$nameID"},
         connectFromField: {"md","nameID"},
         connectToField: {"mdDep","idDep"},
         as: "reportingHierarchy"
      }
   }
] )

db.dependances.aggregate( [
{ $match: {'md': {'$ne': 'mdDep'}} },
   {
      $graphLookup: {
         from: "dependances",
         startWith: "$md",
         connectFromField: "md",
         connectToField: "mdDep",
         as: "reportingHierarchy"
      }
   }
] )
