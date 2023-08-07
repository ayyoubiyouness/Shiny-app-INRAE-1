$(function() {
  var currentCount = 0;
  var increment;
  var interval;

  Shiny.addCustomMessageHandler("startCounter", function(message) {
    var countTo = message.countTo;
    var duration = message.duration;

    increment = countTo / (duration / 10); // Increment value per frame

    interval = setInterval(function() {
      currentCount += increment;
      $('#counter').text(Math.round(currentCount));

      if (currentCount >= countTo) {
        clearInterval(interval);
        $('#counter').text(countTo);
      }
    }, 10);
  });

  Shiny.addCustomMessageHandler("stopCounter", function() {
    clearInterval(interval);
  });
});