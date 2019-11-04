function make_slides(f) {
  var   slides = {};

  // slides.auth = slide({
//   	name : "auth",
//   });
  
  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });
  
  slides.i1 = slide({
    name : "i1",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  }); 
     
  slides.practice1 = slide({
    name : "practice1",
    start : function() {
    $(".err").hide();
    $("#practice_err1").hide();
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));      
    },  
    button : function() {
      this.response = $('input[name="practice1"]:checked').val();
      if (this.response == undefined) {
        $(".err").show();
      } else {
        if (this.response == "No") {
          $(".err").hide();
          $("#practice_err1").show();
        } else {
        exp.go();
      }}}
  });

  slides.practice2 = slide({
    name : "practice2",
    start : function() {
    $(".err").hide();
    $("#practice_err2").hide();
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));     
  },
    button : function() {
      this.response = $('input[name="practice2"]:checked').val();
      if (this.response == undefined) {
        $(".err").show();
      } else {
        if (this.response == "Yes") {
          $(".err").hide();
          $("#practice_err2").show();
        } else {
        exp.go();
      }}}
  });  


  slides.block1 = slide({
    name : "block1",
    present : exp.stims_block1,
    start : function() {
      $(".err").hide();
    },
    present_handle : function(stim) {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	    
      this.stim = stim;
    	this.stim.trial_start = Date.now();      
      $(".err").hide();     
      this.response = undefined;
      $('input[name="critical"]:checked').removeAttr("checked");   		 
      console.log(this.stim);    
      if (this.stim.trigger_class == "control") {
      	var utterance = "<strong> What is true: </strong>"+this.stim.utterance+"";
      } else {
      	var utterance = "<strong> What is true: </strong>"+this.stim.name2 + " " + this.stim.utterance+"";
      }
      // var utterance = "<p>"+this.stim.name + ": \"<i>"+this.stim.utterance+"</i>\"</p>" +"<p>"+this.stim.name2 + ": \"<i>Are you sure?</i>\"</p>"+this.stim.name + ": \"<i>Yes, I'm sure that "+this.stim.question+".</i>\""
	  $(".sentence").html(utterance);
	  var question = "";
	  question = "Does it follow that "+this.stim.question+"?";
	  $(".question").html(question);	  
    },

    button : function() {
      this.response = $('input[name="critical"]:checked').val();
      if (this.response == undefined) {
        $(".err").show();
      } else {
        this.log_responses();
        _stream.apply(this);
      }},
    log_responses : function() {
      exp.data_trials.push({
      //"block" : "block1",
      //"question_type" : this.stim.block,      
   	  "slide_number_in_experiment" : exp.phase,
   	  "verb": this.stim.trigger,
   	  "contentNr": this.stim.content,
   	  "content": this.stim.question,
   	  "speakerGender": this.stim.gender,
   	  "subjectGender": this.stim.gender2,
   	  "speakerName": this.stim.name,
   	  "subjectName": this.stim.name2,
   	  "trigger_class": this.stim.trigger_class,   	  
      "response" : this.response,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  }); 
  
  slides.questionaire =  slide({
    name : "questionaire",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
//        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
		american : $("#american").val(),
        //american : $('input[name="american"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
//        education : $("#education").val(),
        comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.finished = slide({
    name : "finished",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {

var speaker_names = _.shuffle([
    {
      "name":"James",
      "gender":"M"
    },
//    {
//      "name":"John",
//      "gender":"M"
//    },
    {
      "name":"Robert",
      "gender":"M"
    },
//     {
//       "name":"Michael",
//       "gender":"M"
//     },
    {
      "name":"William",
      "gender":"M"
    },
    {
      "name":"David",
      "gender":"M"
    },
   {
     "name":"Richard",
     "gender":"M"
   },
 //    {
//       "name":"Joseph",
//       "gender":"M"
//     },
//     {
//       "name":"Charles",
//       "gender":"M"
//     },
    {
      "name":"Thomas",
      "gender":"M"
    },
    {
      "name":"Christopher",
      "gender":"M"
    },
 //    {
//       "name":"Daniel",
//       "gender":"M"
//     },
    {
      "name":"Matthew",
      "gender":"M"
    },
//    {
//      "name":"Donald",
//      "gender":"M"
//    },
//     {
//       "name":"Anthony",
//       "gender":"M"
//     },
    {
      "name":"Paul",
      "gender":"M"
    },
   {
     "name":"Mark",
     "gender":"M"
   },
    {
      "name":"George",
      "gender":"M"
    },
    {
      "name":"Steven",
      "gender":"M"
    },
    {
      "name":"Kenneth",
      "gender":"M"
    },
    {
      "name":"Jennifer",
      "gender":"F"
    },
    {
      "name":"Elizabeth",
      "gender":"F"
    },
    {
      "name":"Linda",
      "gender":"F"
    },
//     {
//       "name":"Emily",
//       "gender":"F"
//     },
   {
     "name":"Susan",
     "gender":"F"
   },
    {
      "name":"Margaret",
      "gender":"F"
    },
    {
      "name":"Jessica",
      "gender":"F"
    },
    {
      "name":"Dorothy",
      "gender":"F"
    },
    {
      "name":"Sarah",
      "gender":"F"
    },
    {
      "name":"Karen",
      "gender":"F"
    },
    {
      "name":"Nancy",
      "gender":"F"
    },
    {
      "name":"Betty",
      "gender":"F"
    },
    {
      "name":"Lisa",
      "gender":"F"
    },
    {
      "name":"Sandra",
      "gender":"F"
    },
    {
      "name":"Helen",
      "gender":"F"
    },
    {
      "name":"Ashley",
      "gender":"F"
    },
    {
      "name":"Donna",
      "gender":"F"
    },
    {
      "name":"Kimberly",
      "gender":"F"
    },
    {
      "name":"Carol",
      "gender":"F"
    },
    {
      "name":"Michelle",
      "gender":"F"
    }]);


var female_subject_names = _.shuffle([
//       {
//         "name":"Emily",
//         "gender":"F"
//       },
//    {
//      "name":"Mary",
//      "gender":"F"
//    },
    {
      "name":"Amanda",
      "gender":"F"
    },
    {
      "name":"Melissa",
      "gender":"F"
    },
//     {
//       "name":"Deborah",
//       "gender":"F"
//     },
    {
      "name":"Laura",
      "gender":"F"
    },
    {
      "name":"Stephanie",
      "gender":"F"
    },
    {
      "name":"Rebecca",
      "gender":"F"
    },
    {
      "name":"Sharon",
      "gender":"F"
    },
    {
      "name":"Cynthia",
      "gender":"F"
    },
    {
      "name":"Kathleen",
      "gender":"F"
    },
    {
      "name":"Ruth",
      "gender":"F"
    },
   {
     "name":"Anna",
     "gender":"F"
   },
    {
      "name":"Shirley",
      "gender":"F"
    },
    {
      "name":"Amy",
      "gender":"F"
    },
    {
      "name":"Angela",
      "gender":"F"
    },
    {
      "name":"Virginia",
      "gender":"F"
    },
    {
      "name":"Brenda",
      "gender":"F"
    },
    {
      "name":"Catherine",
      "gender":"F"
    },
    {
      "name":"Nicole",
      "gender":"F"
    },
    {
      "name":"Christina",
      "gender":"F"
    },
    {
      "name":"Janet",
      "gender":"F"
    },
 //    {
//       "name":"Samantha",
//       "gender":"F"
//     },
    {
      "name":"Carolyn",
      "gender":"F"
    },
    {
      "name":"Rachel",
      "gender":"F"
    },
    {
      "name":"Heather",
      "gender":"F"
    },
    {
      "name":"Diane",
      "gender":"F"
    },
    {
      "name":"Joyce",
      "gender":"F"
    },
    {
      "name":"Julie",
      "gender":"F"
    }
//     {
//       "name":"Emma",
//       "gender":"F"
//     }   
]);

var male_subject_names = _.shuffle([
   {
     "name":"Andrew",
     "gender":"M"
   },
    {
      "name":"Edward",
      "gender":"M"
    },
 //    {
//       "name":"Joshua",
//       "gender":"M"
//     },
    {
      "name":"Brian",
      "gender":"M"
    },
    {
      "name":"Kevin",
      "gender":"M"
    },
    {
      "name":"Ronald",
      "gender":"M"
    },
    {
      "name":"Timothy",
      "gender":"M"
    },
 //    {
//       "name":"Jason",
//       "gender":"M"
//     },
    {
      "name":"Jeffrey",
      "gender":"M"
    },
    {
      "name":"Gary",
      "gender":"M"
    },
    {
      "name":"Ryan",
      "gender":"M"
    },
    {
      "name":"Nicholas",
      "gender":"M"
    },
    {
      "name":"Eric",
      "gender":"M"
    },
    {
      "name":"Jacob",
      "gender":"M"
    },
  //   {
//       "name":"Jonathan",
//       "gender":"M"
//     },
    {
      "name":"Larry",
      "gender":"M"
    },
//    {
//      "name":"Frank",
//      "gender":"M"
//    },
    {
      "name":"Scott",
      "gender":"M"
    },
    {
      "name":"Justin",
      "gender":"M"
    },
    {
      "name":"Brandon",
      "gender":"M"
    },
    {
      "name":"Raymond",
      "gender":"M"
    },
    {
      "name":"Gregory",
      "gender":"M"
    },
 //    {
//       "name":"Samuel",
//       "gender":"M"
//     },
    {
      "name":"Benjamin",
      "gender":"M"
    },
    {
      "name":"Patrick",
      "gender":"M"
    },
//    {
//      "name":"Jack",
//      "gender":"M"
//    },
//     {
//       "name":"Dennis",
//       "gender":"M"
//     },
    {
      "name":"Jerry",
      "gender":"M"
    },
    {
      "name":"Alexander",
      "gender":"M"
    },
    {
      "name":"Tyler",
      "gender":"M"
    }
    ]);


var items = _.shuffle([ 
   {
     "trigger":"annoyed",
     "trigger_class":"NonProj"
   }, 
   {
     "trigger":"know",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"discover",
     "trigger_class":"NonProj"
   }, 
   {
     "trigger":"reveal",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"see",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"establish",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"pretend",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"think",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"suggest",
     "trigger_class":"C"
   }, 
   {
     "trigger":"prove",
     "trigger_class":"C"
   }, 
   {
     "trigger":"demonstrate",
     "trigger_class":"C"
   }, 
   {
     "trigger":"say",
     "trigger_class":"C"
   },
   {
     "trigger":"hear",
     "trigger_class":"C"
   },
   {
     "trigger":"confess",
     "trigger_class":"C"
   }, 
   {
     "trigger":"inform_Sam",
     "trigger_class":"C"
   }, 
   {
     "trigger":"announce",
     "trigger_class":"C"
   }, 
   {
     "trigger":"acknowledge",
     "trigger_class":"C"
   },
   {
     "trigger":"admit",
     "trigger_class":"C"
   },
   {
     "trigger":"confirm",
     "trigger_class":"C"
   },
   {
     "trigger":"be_right_that",
     "trigger_class":"C"
   }
 ]);

 var contents = {
 "1": {
  "gender":"f",
  "content":"Mary is pregnant",
  "annoyed":"is annoyed that Mary is pregnant.",
  "know":"knows that Mary is pregnant.",
  "discover":"discovered that Mary is pregnant.",
  "reveal":"revealed that Mary is pregnant.",
  "see" :"saw that Mary is pregnant.",
  "establish":"established that Mary is pregnant.",
  "pretend":"pretended that Mary is pregnant.",
  "think":"thinks that Mary is pregnant.",
  "suggest":"suggested that Mary is pregnant.",
  "prove":"proved that Mary is pregnant.",
  "demonstrate":"demonstrated that Mary is pregnant.",
  "say":"said that Mary is pregnant.",
  "hear":"heard that Mary is pregnant.",
  "confess":"confessed that Mary is pregnant.",
  "inform_Sam":"informed Sam that Mary is pregnant.",
  "announce":"announced that Mary is pregnant.",
  "acknowledge":"acknowledged that Mary is pregnant.",
  "admit":"admitted that Mary is pregnant.",
  "confirm":"confirmed that Mary is pregnant.",
  "be_right_that":"is right that Mary is pregnant."
  },
  "2": {
  "gender":"f",
  "content":"Josie went on vacation to France",
  "annoyed":"is annoyed that Josie went on vacation to France.",
  "know":"knows that Josie went on vacation to France.",
  "discover":"discovered that Josie went on vacation to France.",
  "reveal":"revealed that Josie went on vacation to France.",
  "see" :"saw that Josie went on vacation to France.",
  "establish":"established that Josie went on vacation to France.",
  "pretend":"pretended that Josie went on vacation to France.",
  "think":"thinks that Josie went on vacation to France.",
  "suggest":"suggested that Josie went on vacation to France.",
  "prove":"proved that Josie went on vacation to France.",
  "demonstrate":"demonstrated that Josie went on vacation to France.",
  "say":"said that Josie went on vacation to France.",
  "hear":"heard that Josie went on vacation to France.",
  "confess":"confessed that Josie went on vacation to France.",
  "inform_Sam":"informed Sam that Josie went on vacation to France.",
  "announce":"announced that Josie went on vacation to France.",
  "acknowledge":"acknowledged that Josie went on vacation to France.",
  "admit":"admitted that Josie went on vacation to France.",
  "confirm":"confirmed that Josie went on vacation to France.",
  "be_right_that":"is right that Josie went on vacation to France."
  },
  "3": {
  "gender":"f",
  "content":"Emma studied on Saturday morning",
  "annoyed":"is annoyed that Emma studied on Saturday morning.",
  "know":"knows that Emma studied on Saturday morning.",
  "discover":"discovered that Emma studied on Saturday morning.",
  "reveal":"revealed that Emma studied on Saturday morning.",
  "see" :"saw that Emma studied on Saturday morning.",
  "establish":"established that Emma studied on Saturday morning.",
  "pretend":"pretended that Emma studied on Saturday morning.",
  "think":"thinks that Emma studied on Saturday morning.",
  "suggest":"suggested that Emma studied on Saturday morning.",
  "prove":"proved that Emma studied on Saturday morning.",
  "demonstrate":"demonstrated that Emma studied on Saturday morning.",
  "say":"said that Emma studied on Saturday morning.",
  "hear":"heard that Emma studied on Saturday morning.",
  "confess":"confessed that Emma studied on Saturday morning.",
  "inform_Sam":"informed Sam that Emma studied on Saturday morning.",
  "announce":"announced that Emma studied on Saturday morning.",
  "acknowledge":"acknowledged that Emma studied on Saturday morning.",
  "admit":"admitted that Emma studied on Saturday morning.",
  "confirm":"confirmed that Emma studied on Saturday morning.",
  "be_right_that":"is right that Emma studied on Saturday morning."
  },
  "4": {
  "gender":"f",
  "content":"Olivia sleeps until noon",
  "annoyed":"is annoyed that Olivia sleeps until noon.",
  "know":"knows that Olivia sleeps until noon.",
  "discover":"discovered that Olivia sleeps until noon.",
  "reveal":"revealed that Olivia sleeps until noon.",
  "see" :"saw that Olivia sleeps until noon.",
  "establish":"established that Olivia sleeps until noon.",
  "pretend":"pretended that Olivia sleeps until noon.",
  "think":"thinks that Olivia sleeps until noon.",
  "suggest":"suggested that Olivia sleeps until noon.",
  "prove":"proved that Olivia sleeps until noon.",
  "demonstrate":"demonstrated that Olivia sleeps until noon.",
  "say":"said that Olivia sleeps until noon.",
  "hear":"heard that Olivia sleeps until noon.",
  "confess":"confessed that Olivia sleeps until noon.",
  "inform_Sam":"informed Sam that Olivia sleeps until noon.",
  "announce":"announced that Olivia sleeps until noon.",
  "acknowledge":"acknowledged that Olivia sleeps until noon.",
  "admit":"admitted that Olivia sleeps until noon.",
  "confirm":"confirmed that Olivia sleeps until noon.",
  "be_right_that":"is right that Olivia sleeps until noon."
  },
  "5": {
  "gender":"f",
  "content":"Sophia got a tattoo",
  "annoyed":"is annoyed that Sophia got a tattoo.",
  "know":"knows that Sophia got a tattoo.",
  "discover":"discovered that Sophia got a tattoo.",
  "reveal":"revealed that Sophia got a tattoo.",
  "see" :"saw that Sophia got a tattoo.",
  "establish":"established that Sophia got a tattoo.",
  "pretend":"pretended that Sophia got a tattoo.",
  "think":"thinks that Sophia got a tattoo.",
  "suggest":"suggested that Sophia got a tattoo.",
  "prove":"proved that Sophia got a tattoo.",
  "demonstrate":"demonstrated that Sophia got a tattoo.",
  "say":"said that Sophia got a tattoo.",
  "hear":"heard that Sophia got a tattoo.",
  "confess":"confessed that Sophia got a tattoo.",
  "inform_Sam":"informed Sam that Sophia got a tattoo.",
  "announce":"announced that Sophia got a tattoo.",
  "acknowledge":"acknowledged that Sophia got a tattoo.",
  "admit":"admitted that Sophia got a tattoo.",
  "confirm":"confirmed that Sophia got a tattoo.",
  "be_right_that":"is right that Sophia got a tattoo."
  },
  "6": {
  "gender":"f",
  "content":"Mia drank 2 cocktails last night",
  "annoyed":"is annoyed that Mia drank 2 cocktails last night.",
  "know":"knows that Mia drank 2 cocktails last night.",
  "discover":"discovered that Mia drank 2 cocktails last night.",
  "reveal":"revealed that Mia drank 2 cocktails last night.",
  "see" :"saw that Mia drank 2 cocktails last night.",
  "establish":"established that Mia drank 2 cocktails last night.",
  "pretend":"pretended that Mia drank 2 cocktails last night.",
  "think":"thinks that Mia drank 2 cocktails last night.",
  "suggest":"suggested that Mia drank 2 cocktails last night.",
  "prove":"proved that Mia drank 2 cocktails last night.",
  "demonstrate":"demonstrated that Mia drank 2 cocktails last night.",
  "say":"said that Mia drank 2 cocktails last night.",
  "hear":"heard that Mia drank 2 cocktails last night.",
  "confess":"confessed that Mia drank 2 cocktails last night.",
  "inform_Sam":"informed Sam that Mia drank 2 cocktails last night.",
  "announce":"announced that Mia drank 2 cocktails last night.",
  "acknowledge":"acknowledged that Mia drank 2 cocktails last night.",
  "admit":"admitted that Mia drank 2 cocktails last night.",
  "confirm":"confirmed that Mia drank 2 cocktails last night.",
  "be_right_that":"is right that Mia drank 2 cocktails last night."
  },
  "7": {
  "gender":"f",
  "content":"Isabella ate a steak on Sunday",
  "annoyed":"is annoyed that Isabella ate a steak on Sunday.",
  "know":"knows that Isabella ate a steak on Sunday.",
  "discover":"discovered that Isabella ate a steak on Sunday.",
  "reveal":"revealed that Isabella ate a steak on Sunday.",
  "see" :"saw that Isabella ate a steak on Sunday.",
  "establish":"established that Isabella ate a steak on Sunday.",
  "pretend":"pretended that Isabella ate a steak on Sunday.",
  "think":"thinks that Isabella ate a steak on Sunday.",
  "suggest":"suggested that Isabella ate a steak on Sunday.",
  "prove":"proved that Isabella ate a steak on Sunday.",
  "demonstrate":"demonstrated that Isabella ate a steak on Sunday.",
  "say":"said that Isabella ate a steak on Sunday.",
  "hear":"heard that Isabella ate a steak on Sunday.",
  "confess":"confessed that Isabella ate a steak on Sunday.",
  "inform_Sam":"informed Sam that Isabella ate a steak on Sunday.",
  "announce":"announced that Isabella ate a steak on Sunday.",
  "acknowledge":"acknowledged that Isabella ate a steak on Sunday.",
  "admit":"admitted that Isabella ate a steak on Sunday.",
  "confirm":"confirmed that Isabella ate a steak on Sunday.",
  "be_right_that":"is right that Isabella ate a steak on Sunday."
  },
  "8": {
  "gender":"f",
  "content":"Emily bought a car yesterday",
  "annoyed":"is annoyed that Emily bought a car yesterday.",
  "know":"knows that Emily bought a car yesterday.",
  "discover":"discovered that Emily bought a car yesterday.",
  "reveal":"revealed that Emily bought a car yesterday.",
  "see" :"saw that Emily bought a car yesterday.",
  "establish":"established that Emily bought a car yesterday.",
  "pretend":"pretended that Emily bought a car yesterday.",
  "think":"thinks that Emily bought a car yesterday.",
  "suggest":"suggested that Emily bought a car yesterday.",
  "prove":"proved that Emily bought a car yesterday.",
  "demonstrate":"demonstrated that Emily bought a car yesterday.",
  "say":"said that Emily bought a car yesterday.",
  "hear":"heard that Emily bought a car yesterday.",
  "confess":"confessed that Emily bought a car yesterday.",
  "inform_Sam":"informed Sam that Emily bought a car yesterday.",
  "announce":"announced that Emily bought a car yesterday.",
  "acknowledge":"acknowledged that Emily bought a car yesterday.",
  "admit":"admitted that Emily bought a car yesterday.",
  "confirm":"confirmed that Emily bought a car yesterday.",
  "be_right_that":"is right that Emily bought a car yesterday."
  },
  "9": {
  "gender":"f",
  "content":"Grace visited her sister",
  "annoyed":"is annoyed that Grace visited her sister.",
  "know":"knows that Grace visited her sister.",
  "discover":"discovered that Grace visited her sister.",
  "reveal":"revealed that Grace visited her sister.",
  "see" :"saw that Grace visited her sister.",
  "establish":"established that Grace visited her sister.",
  "pretend":"pretended that Grace visited her sister.",
  "think":"thinks that Grace visited her sister.",
  "suggest":"suggested that Grace visited her sister.",
  "prove":"proved that Grace visited her sister.",
  "demonstrate":"demonstrated that Grace visited her sister.",
  "say":"said that Grace visited her sister.",
  "hear":"heard that Grace visited her sister.",
  "confess":"confessed that Grace visited her sister.",
  "inform_Sam":"informed Sam that Grace visited her sister.",
  "announce":"announced that Grace visited her sister.",
  "acknowledge":"acknowledged that Grace visited her sister.",
  "admit":"admitted that Grace visited her sister.",
  "confirm":"confirmed that Grace visited her sister.",
  "be_right_that":"is right that Grace visited her sister."
  },
  "10": {
  "gender":"f",
  "content":"Zoe calculated the tip",
  "annoyed":"is annoyed that Zoe calculated the tip.",
  "know":"knows that Zoe calculated the tip.",
  "discover":"discovered that Zoe calculated the tip.",
  "reveal":"revealed that Zoe calculated the tip.",
  "see" :"saw that Zoe calculated the tip.",
  "establish":"established that Zoe calculated the tip.",
  "pretend":"pretended that Zoe calculated the tip.",
  "think":"thinks that Zoe calculated the tip.",
  "suggest":"suggested that Zoe calculated the tip.",
  "prove":"proved that Zoe calculated the tip.",
  "demonstrate":"demonstrated that Zoe calculated the tip.",
  "say":"said that Zoe calculated the tip.",
  "hear":"heard that Zoe calculated the tip.",
  "confess":"confessed that Zoe calculated the tip.",
  "inform_Sam":"informed Sam that Zoe calculated the tip.",
  "announce":"announced that Zoe calculated the tip.",
  "acknowledge":"acknowledged that Zoe calculated the tip.",
  "admit":"admitted that Zoe calculated the tip.",
  "confirm":"confirmed that Zoe calculated the tip.",
  "be_right_that":"is right that Zoe calculated the tip."
  },
  "11": {
  "gender":"m",
  "content":"Danny ate the last cupcake",
  "annoyed":"is annoyed that Danny ate the last cupcake.",
  "know":"knows that Danny ate the last cupcake.",
  "discover":"discovered that Danny ate the last cupcake.",
  "reveal":"revealed that Danny ate the last cupcake.",
  "see" :"saw that Danny ate the last cupcake.",
  "establish":"established that Danny ate the last cupcake.",
  "pretend":"pretended that Danny ate the last cupcake.",
  "think":"thinks that Danny ate the last cupcake.",
  "suggest":"suggested that Danny ate the last cupcake.",
  "prove":"proved that Danny ate the last cupcake.",
  "demonstrate":"demonstrated that Danny ate the last cupcake.",
  "say":"said that Danny ate the last cupcake.",
  "hear":"heard that Danny ate the last cupcake.",
  "confess":"confessed that Danny ate the last cupcake.",
  "inform_Sam":"informed Sam that Danny ate the last cupcake.",
  "announce":"announced that Danny ate the last cupcake.",
  "acknowledge":"acknowledged that Danny ate the last cupcake.",
  "admit":"admitted that Danny ate the last cupcake.",
  "confirm":"confirmed that Danny ate the last cupcake.",
  "be_right_that":"is right that Danny ate the last cupcake."
  },
  "12": {
  "gender":"m",
  "content":"Frank got a cat",
  "annoyed":"is annoyed that Frank got a cat.",
  "know":"knows that Frank got a cat.",
  "discover":"discovered that Frank got a cat.",
  "reveal":"revealed that Frank got a cat.",
  "see" :"saw that Frank got a cat.",
  "establish":"established that Frank got a cat.",
  "pretend":"pretended that Frank got a cat.",
  "think":"thinks that Frank got a cat.",
  "suggest":"suggested that Frank got a cat.",
  "prove":"proved that Frank got a cat.",
  "demonstrate":"demonstrated that Frank got a cat.",
  "say":"said that Frank got a cat.",
  "hear":"heard that Frank got a cat.",
  "confess":"confessed that Frank got a cat.",
  "inform_Sam":"informed Sam that Frank got a cat.",
  "announce":"announced that Frank got a cat.",
  "acknowledge":"acknowledged that Frank got a cat.",
  "admit":"admitted that Frank got a cat.",
  "confirm":"confirmed that Frank got a cat.",
  "be_right_that":"is right that Frank got a cat."
  },
  "13": {
  "gender":"m",
  "content":"Jackson ran 10 miles",
  "annoyed":"is annoyed that Jackson ran 10 miles.",
  "know":"knows that Jackson ran 10 miles.",
  "discover":"discovered that Jackson ran 10 miles.",
  "reveal":"revealed that Jackson ran 10 miles.",
  "see" :"saw that Jackson ran 10 miles.",
  "establish":"established that Jackson ran 10 miles.",
  "pretend":"pretended that Jackson ran 10 miles.",
  "think":"thinks that Jackson ran 10 miles.",
  "suggest":"suggested that Jackson ran 10 miles.",
  "prove":"proved that Jackson ran 10 miles.",
  "demonstrate":"demonstrated that Jackson ran 10 miles.",
  "say":"said that Jackson ran 10 miles.",
  "hear":"heard that Jackson ran 10 miles.",
  "confess":"confessed that Jackson ran 10 miles.",
  "inform_Sam":"informed Sam that Jackson ran 10 miles.",
  "announce":"announced that Jackson ran 10 miles.",
  "acknowledge":"acknowledged that Jackson ran 10 miles.",
  "admit":"admitted that Jackson ran 10 miles.",
  "confirm":"confirmed that Jackson ran 10 miles.",
  "be_right_that":"is right that Jackson ran 10 miles."
  },
  "14": {
  "gender":"m",
  "content":"Jayden rented a car",
  "annoyed":"is annoyed that Jayden rented a car.",
  "know":"knows that Jayden rented a car.",
  "discover":"discovered that Jayden rented a car.",
  "reveal":"revealed that Jayden rented a car.",
  "see" :"saw that Jayden rented a car.",
  "establish":"established that Jayden rented a car.",
  "pretend":"pretended that Jayden rented a car.",
  "think":"thinks that Jayden rented a car.",
  "suggest":"suggested that Jayden rented a car.",
  "prove":"proved that Jayden rented a car.",
  "demonstrate":"demonstrated that Jayden rented a car.",
  "say":"said that Jayden rented a car.",
  "hear":"heard that Jayden rented a car.",
  "confess":"confessed that Jayden rented a car.",
  "inform_Sam":"informed Sam that Jayden rented a car.",
  "announce":"announced that Jayden rented a car.",
  "acknowledge":"acknowledged that Jayden rented a car.",
  "admit":"admitted that Jayden rented a car.",
  "confirm":"confirmed that Jayden rented a car.",
  "be_right_that":"is right that Jayden rented a car."
  },
  "15": {
  "gender":"m",
  "content":"Tony had a drink last night",
  "annoyed":"is annoyed that Tony had a drink last night.",
  "know":"knows that Tony had a drink last night.",
  "discover":"discovered that Tony had a drink last night.",
  "reveal":"revealed that Tony had a drink last night.",
  "see" :"saw that Tony had a drink last night.",
  "establish":"established that Tony had a drink last night.",
  "pretend":"pretended that Tony had a drink last night.",
  "think":"thinks that Tony had a drink last night.",
  "suggest":"suggested that Tony had a drink last night.",
  "prove":"proved that Tony had a drink last night.",
  "demonstrate":"demonstrated that Tony had a drink last night.",
  "say":"said that Tony had a drink last night.",
  "hear":"heard that Tony had a drink last night.",
  "confess":"confessed that Tony had a drink last night.",
  "inform_Sam":"informed Sam that Tony had a drink last night.",
  "announce":"announced that Tony had a drink last night.",
  "acknowledge":"acknowledged that Tony had a drink last night.",
  "admit":"admitted that Tony had a drink last night.",
  "confirm":"confirmed that Tony had a drink last night.",
  "be_right_that":"is right that Tony had a drink last night."
  },
  "16": {
  "gender":"m",
  "content":"Josh learned to ride a bike yesterday",
  "annoyed":"is annoyed that Josh learned to ride a bike yesterday.",
  "know":"knows that Josh learned to ride a bike yesterday.",
  "discover":"discovered that Josh learned to ride a bike yesterday.",
  "reveal":"revealed that Josh learned to ride a bike yesterday.",
  "see" :"saw that Josh learned to ride a bike yesterday.",
  "establish":"established that Josh learned to ride a bike yesterday.",
  "pretend":"pretended that Josh learned to ride a bike yesterday.",
  "think":"thinks that Josh learned to ride a bike yesterday.",
  "suggest":"suggested that Josh learned to ride a bike yesterday.",
  "prove":"proved that Josh learned to ride a bike yesterday.",
  "demonstrate":"demonstrated that Josh learned to ride a bike yesterday.",
  "say":"said that Josh learned to ride a bike yesterday.",
  "hear":"heard that Josh learned to ride a bike yesterday.",
  "confess":"confessed that Josh learned to ride a bike yesterday.",
  "inform_Sam":"informed Sam that Josh learned to ride a bike yesterday.",
  "announce":"announced that Josh learned to ride a bike yesterday.",
  "acknowledge":"acknowledged that Josh learned to ride a bike yesterday.",
  "admit":"admitted that Josh learned to ride a bike yesterday.",
  "confirm":"confirmed that Josh learned to ride a bike yesterday.",
  "be_right_that":"is right that Josh learned to ride a bike yesterday."
  },
  "17": {
  "gender":"m",
  "content":"Owen shoveled snow last winter",
  "annoyed":"is annoyed that Owen shoveled snow last winter.",
  "know":"knows that Owen shoveled snow last winter.",
  "discover":"discovered that Owen shoveled snow last winter.",
  "reveal":"revealed that Owen shoveled snow last winter.",
  "see" :"saw that Owen shoveled snow last winter.",
  "establish":"established that Owen shoveled snow last winter.",
  "pretend":"pretended that Owen shoveled snow last winter.",
  "think":"thinks that Owen shoveled snow last winter.",
  "suggest":"suggested that Owen shoveled snow last winter.",
  "prove":"proved that Owen shoveled snow last winter.",
  "demonstrate":"demonstrated that Owen shoveled snow last winter.",
  "say":"said that Owen shoveled snow last winter.",
  "hear":"heard that Owen shoveled snow last winter.",
  "confess":"confessed that Owen shoveled snow last winter.",
  "inform_Sam":"informed Sam that Owen shoveled snow last winter.",
  "announce":"announced that Owen shoveled snow last winter.",
  "acknowledge":"acknowledged that Owen shoveled snow last winter.",
  "admit":"admitted that Owen shoveled snow last winter.",
  "confirm":"confirmed that Owen shoveled snow last winter.",
  "be_right_that":"is right that Owen shoveled snow last winter."
  },
  "18": {
  "gender":"m",
  "content":"Julian dances salsa",
  "annoyed":"is annoyed that Julian dances salsa.",
  "know":"knows that Julian dances salsa.",
  "discover":"discovered that Julian dances salsa.",
  "reveal":"revealed that Julian dances salsa.",
  "see" :"saw that Julian dances salsa.",
  "establish":"established that Julian dances salsa.",
  "pretend":"pretended that Julian dances salsa.",
  "think":"thinks that Julian dances salsa.",
  "suggest":"suggested that Julian dances salsa.",
  "prove":"proved that Julian dances salsa.",
  "demonstrate":"demonstrated that Julian dances salsa.",
  "say":"said that Julian dances salsa.",
  "hear":"heard that Julian dances salsa.",
  "confess":"confessed that Julian dances salsa.",
  "inform_Sam":"informed Sam that Julian dances salsa.",
  "announce":"announced that Julian dances salsa.",
  "acknowledge":"acknowledged that Julian dances salsa.",
  "admit":"admitted that Julian dances salsa.",
  "confirm":"confirmed that Julian dances salsa.",
  "be_right_that":"is right that Julian dances salsa."
  },
  "19": {
  "gender":"m",
  "content":"Jon walks to work",
  "annoyed":"is annoyed that Jon walks to work.",
  "know":"knows that Jon walks to work.",
  "discover":"discovered that Jon walks to work.",
  "reveal":"revealed that Jon walks to work.",
  "see" :"saw that Jon walks to work.",
  "establish":"established that Jon walks to work.",
  "pretend":"pretended that Jon walks to work.",
  "think":"thinks that Jon walks to work.",
  "suggest":"suggested that Jon walks to work.",
  "prove":"proved that Jon walks to work.",
  "demonstrate":"demonstrated that Jon walks to work.",
  "say":"said that Jon walks to work.",
  "hear":"heard that Jon walks to work.",
  "confess":"confessed that Jon walks to work.",
  "inform_Sam":"informed Sam that Jon walks to work.",
  "announce":"announced that Jon walks to work.",
  "acknowledge":"acknowledged that Jon walks to work.",
  "admit":"admitted that Jon walks to work.",
  "confirm":"confirmed that Jon walks to work.",
  "be_right_that":"is right that Jon walks to work."
  },
  "20": {
  "gender":"m",
  "content":"Charley speaks Spanish",
  "annoyed":"is annoyed that Charley speaks Spanish.",
  "know":"knows that Charley speaks Spanish.",
  "discover":"discovered that Charley speaks Spanish.",
  "reveal":"revealed that Charley speaks Spanish.",
  "see" :"saw that Charley speaks Spanish.",
  "establish":"established that Charley speaks Spanish.",
  "pretend":"pretended that Charley speaks Spanish.",
  "think":"thinks that Charley speaks Spanish.",
  "suggest":"suggested that Charley speaks Spanish.",
  "prove":"proved that Charley speaks Spanish.",
  "demonstrate":"demonstrated that Charley speaks Spanish.",
  "say":"said that Charley speaks Spanish.",
  "hear":"heard that Charley speaks Spanish.",
  "confess":"confessed that Charley speaks Spanish.",
  "inform_Sam":"informed Sam that Charley speaks Spanish.",
  "announce":"announced that Charley speaks Spanish.",
  "acknowledge":"acknowledged that Charley speaks Spanish.",
  "admit":"admitte that Charley speaks Spanish.",
  "confirm":"confirmed that Charley speaks Spanish.",
  "be_right_that":"is right that Charley speaks Spanish."
  }
};
  
var items_content_mapping = {
"annoyed":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"know":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"discover":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"reveal":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"see":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"establish":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"pretend":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],  
"think":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"suggest":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"prove":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"demonstrate":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"say":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],	 "hear":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"confess":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],	 "inform_Sam":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"announce":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],	 "acknowledge":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"admit":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],	 "confirm":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"],
"be_right_that":["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"]
};  
 

// get trigger contents
  function getContent(trigger) {
//  		console.log("items_content_mapping before throwing out "+trigger);
//  		console.log(items_content_mapping);
//  		for (var j in items_content_mapping) {  	
//  		console.log("items_content_mapping at "+j);  			
//  		console.log(items_content_mapping[j]);  		
//  		}  		
//  		console.log("items_content_mapping at the trigger before shuffling");
//  		console.log(items_content_mapping[trigger]);  		
  		items_content_mapping[trigger] = _.shuffle(items_content_mapping[trigger]);
//  		console.log("items_content_mapping at the trigger after shuffling");
//  		console.log(items_content_mapping[trigger]);  		  		
//  		console.log("items_content_mapping after shuffling "+trigger);
//  		console.log(items_content_mapping);
  		var content = items_content_mapping[trigger].shift();//items_content_mapping[trigger][0];
//  		console.log("this is the selected content: " + content);
//		var index = items_content_mapping[trigger].indexOf(content);  		
//  		items_content_mapping[trigger] = items_content_mapping[trigger].splice(index,1);
//  		console.log("items_content_mapping at the trigger after throwing it out");
//  		console.log(items_content_mapping[trigger]);  		  		
  		for (var j in items_content_mapping) {
			var index = items_content_mapping[j].indexOf(content);  
//			console.log("the next three lines: the array before removal, the index of content, the array after removal")
//			console.log(items_content_mapping[j]);
//			console.log(index);		
			if (index != -1)
			{			  			
				items_content_mapping[j].splice(index,1);			
			}
//			console.log(items_content_mapping[j]);			
  		}
//  		console.log("items_content_mapping after throwing out "+trigger);
//  		console.log(items_content_mapping);
//  		for (var j in items_content_mapping) {  	
//  		console.log("items_content_mapping at "+j);  			
//  		console.log(items_content_mapping[j]);  		
//  		}   		  		
  		return content;
  	}

// assign contents to triggers
  var trigger_contents = {
  	"annoyed": getContent("annoyed"),  	  	
  	"know": getContent("know"),
  	"discover": getContent("discover"),  	  	
  	"reveal": getContent("reveal"),
  	"see": getContent("see"),
  	"establish": getContent("establish"),
  	"pretend": getContent("pretend"),  	
  	"think": getContent("think"),  	
  	"suggest": getContent("suggest"),
  	"prove": getContent("prove"),
  	"demonstrate": getContent("demonstrate"),
  	"say": getContent("say"),
  	"hear": getContent("hear"),
  	"confess": getContent("confess"),  	
  	"inform_Sam": getContent("inform_Sam"),
  	"announce": getContent("announce"),
  	"acknowledge": getContent("acknowledge"),
  	"admit": getContent("admit"),
  	"confirm": getContent("confirm"),
  	"be_right_that": getContent("be_right_that")
  	};
  	

control_items = [
	{
		"item_id" : "control_good1",
		"short_trigger" : "control_good",
		"utterance" : "Zack bought himself a car this morning.",
		"content" : "Zack owns a car"
	},
	{
		"item_id" : "control_good2",
		"short_trigger" : "control_good",
		"utterance" : "Tara broke the window with a bat.",
		"content" : "the window broke"
	},
	{
		"item_id" : "control_good3",
		"short_trigger" : "control_good",
		"utterance" : "Frederick managed to solve the problem.",
		"content" : "Frederick solved the problem"
	},
	{
		"item_id" : "control_good4",
		"short_trigger" : "control_good",
		"utterance" : "Vanessa happened to look into the mirror.",
		"content" : "Vanessa looked into the mirror"
	},
	{
		"item_id" : "control_bad1",
		"short_trigger" : "control_bad",
		"utterance" : "Dana watched a movie last night.",
		"content" : "Dana wears a wig"
	},
	{
		"item_id" : "control_bad2",
		"short_trigger" : "control_bad",
		"utterance" : "Hendrick is renting an apartment.",
		"content" : "the apartment has a balcony"
	},
	{
		"item_id" : "control_bad3",
		"short_trigger" : "control_bad",
		"utterance" : "Madison was unsuccessful in closing the window.",
		"content" : "Madison closed the window"
	},
	{
		"item_id" : "control_bad4",
		"short_trigger" : "control_bad",
		"utterance" : "Sebastian failed the exam.",
		"content" : "Sebastian did really well on the exam"
	}
];

  function makeControlStim(i) {
    //get item
    var item = control_items[i];
	//get a name to be speaker
    var name_data = speaker_names[i];
    var name = name_data.name;
    var gender = name_data.gender;

    return {
	  "name": name,
	  "name2": "NA",
	  "gender": gender,	
	  "gender2": "NA",  
	  "trigger": item.short_trigger,
	  "short_trigger": item.short_trigger,	  
	  "trigger_class": "control",
      "content": item.item_id,
      "utterance": item.utterance,
      "question": item.content
    }
  }

  function makeStim(i) {
    //get item
    var item = items[i];
	//get a name to be speaker
    var name_data = speaker_names[i];
    var name = name_data.name;
    var gender = name_data.gender;
    
    // get content
    var trigger_cont = trigger_contents[item.trigger];
    var trigger = item.trigger;
    var short_trigger = trigger;
    	

//   console.log("short_trigger: "+short_trigger);
//	console.log("trigger: "+trigger);
	console.log(trigger_cont);
//  console.log("trigger_cont: "+trigger_cont);
//    console.log("utterance: "+contents[trigger_cont][short_trigger]);    
//    console.log(contents[trigger_cont]);    
    var utterance = contents[trigger_cont][short_trigger];
    var question = contents[trigger_cont].content; 
//   console.log(contents[trigger_cont]);
//    console.log(question) 
    //get another name to be subject
    var name_data2 = contents[trigger_cont].gender == "m" ? female_subject_names[i] : male_subject_names[i];
    var name2 = name_data2.name;
    var gender2 = name_data2.gender;
    return {
	  "name": name,
	  "name2": name2,
	  "gender": gender,	
	  "gender2": gender2,  
	  "trigger": item.trigger,
	  "short_trigger": short_trigger,	  
	  "trigger_class": item.trigger_class,
      "content": trigger_cont,
      "utterance": utterance,
      "question": question
    }
  }
  exp.stims_block1 = [];
//   exp.stims_block2 = []; 
  for (var i=0; i<items.length; i++) {
  	var stim = makeStim(i);
//    exp.stims_block1.push(makeStim(i));
	exp.stims_block1.push(jQuery.extend(true, {}, stim));
//	exp.stims_block2.push(jQuery.extend(true, {}, stim));	
  }  

  for (var j=0; j<control_items.length; j++) {
  	var stim = makeControlStim(j);
//    exp.stims_block1.push(makeStim(i));
	exp.stims_block1.push(jQuery.extend(true, {}, stim));
//	exp.stims_block2.push(jQuery.extend(true, {}, stim));	
  }    
  
console.log(exp.stims_block1);
//console.log(exp.stims_block2);   

	exp.stims_block1 = _.shuffle(exp.stims_block1);  
//	exp.stims_block2 = _.shuffle(exp.stims_block2); 
	
// decide which block comes first
//   var block_order = _.shuffle(["ai","projective"]);
//   var block1type = block_order[0];
//   var block2type = block_order[1];  
//   console.log(block_order);
//   console.log(block1type);  
//   console.log(block2type);    
// 
//    for (k in exp.stims_block2) {
//    		exp.stims_block2[k].block = block2type;//block_order[1];   	
//    	}
//    	
//    for (i in exp.stims_block1) {
//    		exp.stims_block1[i].block = block1type;//block_order[0];   	
//    	}


console.log(exp.stims_block1);
//console.log(exp.stims_block2);   	

//  exp.all_stims = [];
//  for (var i=0; i<items.length; i++) {
//    exp.all_stims.push(makeStim(i));
//  }
//
//	for (k in exp.all_stims) {
//		console.log(exp.all_stims[k].content)
//		}

  exp.trials = [];
  exp.catch_trials = [];
  exp.condition = {}; //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "practice1", "practice2", "i1", "block1", 'questionaire', 'finished'];
  //exp.structure=["auth", "i0", "practice1", "practice2", "i1", "block1", 'questionaire', 'finished'];
  
  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

 exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined
                    
   // exp.nQs = 2 + 20 + 1; 
  $(".nQs").html(exp.nQs);

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}