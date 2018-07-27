use logipedia

db.constants.aggregate([
 {
   $group:
    {
      _id: { md: "$md", id: "$id", sys: "$sys" },
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
   db.constants.remove({
       _id: {$in: doc.dups}
   });
})

db.definitions.aggregate([
 {
   $group:
    {
      _id: { md: "$md", id: "$id", sys: "$sys" },
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

db.axioms.aggregate([
 {
   $group:
    {
      _id: { md: "$md", id: "$id", sys: "$sys" },
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
   db.axioms.remove({
       _id: {$in: doc.dups}
   });
})

db.theorems.aggregate([
 {
   $group:
    {
      _id: { md: "$md", id: "$id", sys: "$sys" },
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
   db.theorems.remove({
       _id: {$in: doc.dups}
   });
})

db.idDep.aggregate([
 {
   $group:
    {
      _id: { md: "$md", id: "$id", mdDep: "$mdDep", idDep: "$idDep" },
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
   db.idDep.remove({
       _id: {$in: doc.dups}
   });
})

db.mdDep.aggregate([
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
   db.mdDep.remove({
       _id: {$in: doc.dups}
   });
})

db.mdDep.remove( { $where: "this.mdDep == this.md" } )

