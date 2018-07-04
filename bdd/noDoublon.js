use logipedia

db.parameters.aggregate([
 {
   $group:
    {
      _id: { md: "$md", nameID: "$nameID", langID: "$langID" },
      dups: { $addToSet: "$_id" },
      count: { $sum:1 }
   }
 },
 {
   $match:
     {
       count: {"$gt": 1}
     }
 }
]).forEach(function(doc) {
   doc.dups.shift();
   db.parameters.remove({
       _id: {$in: doc.dups}
   });
})

db.definitions.aggregate([
 {
   $group:
    {
      _id: { md: "$md", nameID: "$nameID", langID: "$langID" },
      dups: { $addToSet: "$_id" },
      count: { $sum:1 }
   }
 },
 {
   $match:
     {
       count: {"$gt": 1}
     }
 }
]).forEach(function(doc) {
   doc.dups.shift();
   db.definitions.remove({
       _id: {$in: doc.dups}
   });
})

db.axiomes.aggregate([
 {
   $group:
    {
      _id: { md: "$md", nameID: "$nameID", langID: "$langID" },
      dups: { $addToSet: "$_id" },
      count: { $sum:1 }
   }
 },
 {
   $match:
     {
       count: {"$gt": 1}
     }
 }
]).forEach(function(doc) {
   doc.dups.shift();
   db.axiomes.remove({
       _id: {$in: doc.dups}
   });
})

db.theoremes.aggregate([
 {
   $group:
    {
      _id: { md: "$md", nameID: "$nameID", langID: "$langID" },
      dups: { $addToSet: "$_id" },
      count: { $sum:1 }
   }
 },
 {
   $match:
     {
       count: {"$gt": 1}
     }
 }
]).forEach(function(doc) {
   doc.dups.shift();
   db.theoremes.remove({
       _id: {$in: doc.dups}
   });
})

db.dependances.aggregate([
 {
   $group:
    {
      _id: { md: "$md", nameID: "$nameID", mdDep: "$mdDep", idDep: "$idDep" },
      dups: { $addToSet: "$_id" },
      count: { $sum:1 }
   }
 },
 {
   $match:
     {
       count: {"$gt": 1}
     }
 }
]).forEach(function(doc) {
   doc.dups.shift();
   db.dependances.remove({
       _id: {$in: doc.dups}
   });
})

db.dependancesMod.aggregate([
 {
   $group:
    {
      _id: { md: "$md", mdDep: "$mdDep"},
      dups: { $addToSet: "$_id" },
      count: { $sum:1 }
   }
 },
 {
   $match:
     {
       count: {"$gt": 1}
     }
 }
]).forEach(function(doc) {
   doc.dups.shift();
   db.dependancesMod.remove({
       _id: {$in: doc.dups}
   });
})

db.dependancesMod.remove( { $where: "this.mdDep == this.md" } )

