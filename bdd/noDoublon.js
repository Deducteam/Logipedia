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
   db.parameters.remove({
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
   db.parameters.remove({
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
   db.parameters.remove({
       _id: {$in: doc.dups}
   });
})
