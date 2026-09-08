function areaCalculator() {
	// Get input
	var inputElement = document.getElementById('inputPixels');
	var inputValue = parseFloat(inputElement.value);

	// Validate input
	if (isNaN(inputValue)) {
		alert('Please enter a valid number.');
		return;
	}

	// Calculate in sqkm and sqmi
	var resultSQKM = inputValue * (50 / (2.808 * 2.808));
	var resultSQMI = resultSQKM / 2.59;

	// Display the result in sqkm
	var outputSQKM = document.getElementById('outputSQKM');
	outputSQKM.textContent = resultSQKM.toFixed(2); // Display result with 2 decimal places
	
	// Display the result in sqmi
	var outputSQMI = document.getElementById('outputSQMI');
	outputSQMI.textContent = resultSQMI.toFixed(2);
}

function clearArea() {
	//Reset input box
	document.getElementById('inputPixels').value = '';
	
	// Reset sqkm value
	document.getElementById('outputSQKM').textContent = 0;
	
	// Reset sqmi value
	document.getElementById('outputSQMI').textContent = 0;
}

function lengthCalculator() {
	// Get input
	var inputElement = document.getElementById('inputLength');
	var inputValue = parseFloat(inputElement.value);

	// Validate input
	if (isNaN(inputValue)) {
		alert('Please enter a valid number.');
		return;
	}

	// Calculate in km and mi
	var resultKM = inputValue * (Math.sqrt(50) / 2.808);
	var resultMI = resultKM / 1.609;

	// Display the result in km
	var outputKM = document.getElementById('outputKM');
	outputKM.textContent = resultKM.toFixed(2); // Display result with 2 decimal places
	
	// Display the result in mi
	var outputMI = document.getElementById('outputMI');
	outputMI.textContent = resultMI.toFixed(2);
}

function clearLength() {
	//Reset input box
	document.getElementById('inputLength').value = '';
	
	// Reset km value
	document.getElementById('outputKM').textContent = 0;
	
	// Reset mi value
	document.getElementById('outputMI').textContent = 0;
}